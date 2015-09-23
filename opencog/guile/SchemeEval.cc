/**
 * SchemeEval.cc
 *
 * Scheme evaluator.  Given strings in scheme, evaluates them with
 * the appropriate Atomspace, etc.
 *
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas
 */

#ifdef HAVE_GUILE

#include <unistd.h>
#include <fcntl.h>

#include <cstddef>
#include <libguile.h>
#include <libguile/backtrace.h>
#include <libguile/debug.h>
#ifndef HAVE_GUILE2
  #include <libguile/lang.h>
#endif
#include <pthread.h>

#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>
#include <opencog/util/platform.h>
#include <opencog/atomspace/TruthValue.h>

#include "SchemeEval.h"
#include "SchemePrimitive.h"
#include "SchemeSmob.h"

using namespace opencog;

std::mutex init_mtx;

/**
 * This init is called once for every time that this class
 * is instantiated -- i.e. it is a per-instance initializer.
 */
void SchemeEval::init(void)
{
	// Arghhh!  Avoid ongoing utf8 fruitcake nutiness in guile-2.0
	scm_c_eval_string ("(setlocale LC_ALL "")\n");

	SchemeSmob::init();
	PrimitiveEnviron::init();

	_in_server = false;
	_in_redirect = 0;
	_in_shell = false;
	_in_eval = false;

	// User error and crash management
	error_string = SCM_EOL;
	error_string = scm_gc_protect_object(error_string);

	captured_stack = SCM_BOOL_F;
	captured_stack = scm_gc_protect_object(captured_stack);

	pexpr = NULL;
	_eval_done = false;
	_poll_done = false;

	_rc = SCM_EOL;
	_rc = scm_gc_protect_object(_rc);

	_gc_ctr = 0;
}

/// When the user is using the guile shell from within the cogserver,
/// We need to capture the output port. It is used by the cogserver
/// shell to provide an async I/O mechanism, so that long-running scheme
/// expressions will have the output captured and displayed as they run,
/// instead of getting it all saved up and only displayed at the very end.
/// But if we are not running in the cogserver, non of this is needed.
///
void SchemeEval::capture_port(void)
{
	// If we've already captured, don't do it again.
	if (_in_server) return;

	// Lock to prevent racey setting of the output port.
	// XXX FIXME This lock is not needed, because in guile 2.2,
	// at least, every thread has its own output port, and so its
	// impossible for two different threads to compete to set the
	// same outport.  Not to sure about guile-2.0, though... so
	// I'm leaving the lock in, for now. Its harmless.
	std::lock_guard<std::mutex> lck(init_mtx);

	// Try again, under the lock this time.
	if (_in_server) return;
	_in_server = true;
	_in_redirect = 1;

	// When running in the cogserver, this pipe will become the output
	// port.  Scheme code will be writing into one end of it, while, in a
	// different thread, we will be sucking it dry, and displaying the
	// contents to the user.
	SCM pair = scm_pipe();
	_pipe = scm_car(pair);
	_pipe = scm_gc_protect_object(_pipe);
	_pipeno = scm_to_int(scm_fileno(_pipe));
	_outport = scm_cdr(pair);
	_outport = scm_gc_protect_object(_outport);
	scm_setvbuf(_outport, scm_from_int (_IONBF), SCM_UNDEFINED);

	// We want non-blocking reads.
	int flags = fcntl(_pipeno, F_GETFL, 0);
	if (flags < 0) flags = 0;
	fcntl(_pipeno, F_SETFL, flags | O_NONBLOCK);
}

/// Use the async I/O mechanism, if we are in the cogserver.
///
/// Note, by the way, that Guile implements the current port as a fluid
/// on each thread. So this save an restore gives us exactly the right
/// per-thread semantics.
void SchemeEval::redirect_output(void)
{
	_in_redirect++;
	if (1 < _in_redirect) return;
	capture_port();

	// Output ports for side-effects.
	_saved_outport = scm_current_output_port();
	_saved_outport = scm_gc_protect_object(_saved_outport);

	scm_set_current_output_port(_outport);
}

void SchemeEval::restore_output(void)
{
	_in_redirect --;
	if (0 < _in_redirect) return;

	// Restore the previous outport (if its still alive)
	if (scm_is_false(scm_port_closed_p(_saved_outport)))
		scm_set_current_output_port(_saved_outport);
	scm_gc_unprotect_object(_saved_outport);
}

/// Discard all chars in the outport.
void SchemeEval::drain_output(void)
{
	// read and discard.
	poll_port();
}

void * SchemeEval::c_wrap_init(void *p)
{
	SchemeEval *self = (SchemeEval *) p;
	self->init();
	return self;
}

void SchemeEval::finish(void)
{
	scm_gc_unprotect_object(_rc);

	std::lock_guard<std::mutex> lck(init_mtx);

	// If we had once set up the async I/O, the release it.
	if (_in_server)
	{
		scm_close_port(_outport);
		scm_gc_unprotect_object(_outport);

		scm_close_port(_pipe);
		scm_gc_unprotect_object(_pipe);
	}

	scm_gc_unprotect_object(error_string);
	scm_gc_unprotect_object(captured_stack);

	// Force garbage collection
	scm_gc();
}

void * SchemeEval::c_wrap_finish(void *p)
{
	SchemeEval *self = (SchemeEval *) p;
	self->finish();
	return self;
}

// The following two routines are needed to avoid bad garbage collection
// of anything we've kept in the object.
void SchemeEval::set_captured_stack(SCM newstack)
{
	// protect before unprotecting, to avoid multi-threaded races.
	SCM oldstack = captured_stack;
	captured_stack = scm_gc_protect_object(newstack);
	scm_gc_unprotect_object(oldstack);
}

void SchemeEval::set_error_string(SCM newerror)
{
	SCM olderror = error_string;
	error_string = scm_gc_protect_object(newerror);
	scm_gc_unprotect_object(olderror);
}

static std::atomic_flag eval_is_inited = ATOMIC_FLAG_INIT;
static thread_local bool thread_is_inited = false;

#ifndef HAVE_GUILE2
	#define WORK_AROUND_GUILE_185_BUG
#endif
#ifdef WORK_AROUND_GUILE_185_BUG
/* There's a bug in guile-1.8.5, where the second and subsequent
 * threads run in guile mode with a bogus/broken current-module.
 * This cannot be worked around by anything as simple as saying
 * "(set-current-module the-root-module)" because dynwind undoes
 * any module-setting that we do.
 *
 * So we work around it here, by explicitly setting the module
 * outside of a dynwind context.
 */
static SCM guile_user_module;

static void * do_bogus_scm(void *p)
{
	scm_c_eval_string ("(+ 2 2)\n");
	return p;
}
#endif /* WORK_AROUND_GUILE_185_BUG */

#ifndef HAVE_GUILE2
	#define WORK_AROUND_GUILE_THREADING_BUG
#endif
#ifdef WORK_AROUND_GUILE_THREADING_BUG
/* There are bugs in guile-1.8.6 and earlier that prevent proper
 * multi-threaded operation. Currently, the most serious of these is
 * a parallel-define bug, documented in
 * https://savannah.gnu.org/bugs/index.php?24867
 *
 * Until that bug is fixed and released, this work-around is needed.
 * The work-around serializes all guile-mode thread execution, by
 * means of a mutex lock.
 *
 * As of December 2013, the bug still seems to be there: the test
 * case provided in the bug report crashes, when linked against
 * guile-2.0.5 and gc-7.1 from Ubuntu Precise.
 *
 * Its claimed that the bug only happens for top-level defines.
 * Thus, in principle, threading should be OK after all scripts have
 * been loaded.
 *
 * FWIW, the unit test MultiThreadUTest tests atom creation in multiple
 * threads. As of 29 Nov 2014, it passes, for me, using guile-2.0.9
 * which is the stock version of guile in Mint Qiana 17 aka Ubuntu 14.04
 */
static pthread_mutex_t serialize_lock;
static pthread_key_t ser_key = 0;
#endif /* WORK_AROUND_GUILE_THREADING_BUG */

// Initialization that needs to be performed only once, for the entire
// process.
static void init_only_once(void)
{
	if (eval_is_inited.test_and_set()) return;

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	pthread_mutex_init(&serialize_lock, NULL);
	pthread_key_create(&ser_key, NULL);
	pthread_setspecific(ser_key, (const void *) 0x0);
#endif /* WORK_AROUND_GUILE_THREADING_BUG */
#ifdef WORK_AROUND_GUILE_185_BUG
	scm_with_guile(do_bogus_scm, NULL);
	guile_user_module = scm_current_module();
#endif /* WORK_AROUND_GUILE_185_BUG */
}

#ifdef WORK_AROUND_GUILE_THREADING_BUG

/**
 * This lock primitive allow nested locks within one thread,
 * but prevents concurrent threads from running.
 */
void SchemeEval::thread_lock(void)
{
	long cnt = (long) pthread_getspecific(ser_key);
	if (0 >= cnt)
	{
		pthread_mutex_lock(&serialize_lock);
	}
	cnt ++;
	pthread_setspecific(ser_key, (const void *) cnt);
}

void SchemeEval::thread_unlock(void)
{
	long cnt = (long) pthread_getspecific(ser_key);
	cnt --;
	pthread_setspecific(ser_key, (const void *) cnt);
	if (0 >= cnt)
	{
		pthread_mutex_unlock(&serialize_lock);
	}
}
#endif

SchemeEval::SchemeEval(AtomSpace* as)
{
	init_only_once();

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_lock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */
	atomspace = as;

	scm_with_guile(c_wrap_init, this);

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_unlock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */

}

/* This should be called once for every new thread. */
void SchemeEval::per_thread_init(void)
{
	/* Avoid more than one call per thread. */
	if (thread_is_inited) return;
	thread_is_inited = true;

#ifdef WORK_AROUND_GUILE_185_BUG
	scm_set_current_module(guile_user_module);
#endif /* WORK_AROUND_GUILE_185_BUG */

	// Arghhh!  Avoid ongoing utf8 fruitcake nutiness in guile-2.0
	scm_c_eval_string ("(setlocale LC_ALL "")\n");
}

SchemeEval::~SchemeEval()
{
	scm_with_guile(c_wrap_finish, this);
}

/* ============================================================== */

std::string SchemeEval::prt(SCM node)
{
	if (SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, node))
	{
		return SchemeSmob::misc_to_string(node);
	}
	else if (scm_is_eq(node, SCM_UNSPECIFIED))
	{
		return "";
	}
	else
	{
		// Let SCM display do the rest of the work.
		SCM port = scm_open_output_string();
		scm_display (node, port);
		SCM rc = scm_get_output_string(port);
		char * str = scm_to_utf8_string(rc);
		std::string rv = str;
		free(str);
		scm_close_port(port);
		return rv;
	}

	return "";
}

/* ============================================================== */

SCM SchemeEval::preunwind_handler_wrapper (void *data, SCM tag, SCM throw_args)
{
	SchemeEval *ss = (SchemeEval *) data;
	return ss->preunwind_handler(tag, throw_args);
}

SCM SchemeEval::catch_handler_wrapper (void *data, SCM tag, SCM throw_args)
{
	SchemeEval *ss = (SchemeEval *) data;
	return ss->catch_handler(tag, throw_args);
}

SCM SchemeEval::preunwind_handler (SCM tag, SCM throw_args)
{
	// We can only record the stack before it is unwound.
	// The normal catch handler body runs only *after* the stack
	// has been unwound.
	set_captured_stack(scm_make_stack(SCM_BOOL_T, SCM_EOL));
	return SCM_EOL;
}

SCM SchemeEval::catch_handler (SCM tag, SCM throw_args)
{
	// Check for read error. If a read error, then wait for user to correct it.
	SCM re = scm_symbol_to_string(tag);
	char * restr = scm_to_utf8_string(re);
	_pending_input = false;

	if (0 == strcmp(restr, "read-error"))
	{
		_pending_input = true;
		free(restr);
		return SCM_EOL;
	}

	// Check for a simple flow-control directive: i.e. just return to
	// the C code from anywhere within the scheme code.
	if (0 == strcmp(restr, "cog-yield"))
	{
		free(restr);
		return SCM_CAR(throw_args);
	}

	// If it's not a read error, and it's not flow-control,
	// then its a regular error; report it.
	_caught_error = true;

	/* get string port into which we write the error message and stack. */
	SCM port = scm_open_output_string();

	if (scm_is_true(scm_list_p(throw_args)) && (scm_ilength(throw_args) >= 1))
	{
		long nargs = scm_ilength(throw_args);
		SCM subr	= SCM_CAR (throw_args);
		SCM message = SCM_EOL;
		if (nargs >= 2)
			message = SCM_CADR (throw_args);
		SCM parts = SCM_EOL;
		if (nargs >= 3)
			parts = SCM_CADDR (throw_args);
		SCM rest	= SCM_EOL;
		if (nargs >= 4)
			rest = SCM_CADDDR (throw_args);

		if (scm_is_true (captured_stack))
		{
			SCM highlights;

			if (scm_is_eq (tag, scm_arg_type_key) ||
				scm_is_eq (tag, scm_out_of_range_key))
				highlights = rest;
			else
				highlights = SCM_EOL;

			scm_puts ("Backtrace:\n", port);
			scm_display_backtrace_with_highlights (captured_stack, port,
			                                       SCM_BOOL_F, SCM_BOOL_F,
			                                       highlights);
			scm_newline (port);
		}
#ifdef HAVE_GUILE2
		if (SCM_STACK_LENGTH (captured_stack))
			set_captured_stack(scm_stack_ref (captured_stack, SCM_INUM0));
#endif
		scm_display_error (captured_stack, port, subr, message, parts, rest);
	}
	else
	{
		scm_puts ("ERROR: throw args are unexpectedly short!\n", port);
	}
	scm_puts("ABORT: ", port);
	scm_puts(restr, port);
	free(restr);

	set_error_string(scm_get_output_string(port));
	scm_close_port(port);
	return SCM_BOOL_F;
}

/* ============================================================== */
/**
 * Evaluate a scheme expression.
 *
 * This evaluator is tailored for being invoked by a shell, in several
 * different ways:
 *
 * 1) It buffers up incomplete, line-by-line input, until there's
 *	been enough input received to evaluate without error.
 * 2) It catches errors, and prints the catch in a reasonably nicely
 *	formatted way.
 * 3) It converts any returned scheme expressions to a string, for easy
 *	printing.
 * 4) It concatenates any data sent to the scheme output port (e.g.
 *	printed output from the scheme (display) function) to the returned
 *	string.
 *
 * An "unforgiving" evaluator, with none of these amenities, can be
 * found in eval_h(), below.
 */
void SchemeEval::eval_expr(const std::string &expr)
{
	// If we are recursing, then we already are in the guile
	// environment, and don't need to do any additional setup.
	// Just go.
	if (_in_eval) {
	   do_eval(expr);
		return;
	}

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_lock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */

	pexpr = &expr;
	_in_shell = true;
	_in_eval = true;
	scm_with_guile(c_wrap_eval, this);
	_in_eval = false;
	_in_shell = false;

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_unlock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */
}

void* SchemeEval::c_wrap_poll(void* p)
{
	SchemeEval* self = (SchemeEval*) p;
	self->answer = self->do_poll_result();
	return p;
}

std::string SchemeEval::poll_result()
{
	scm_with_guile(c_wrap_poll, this);
	return answer;
}

void* SchemeEval::c_wrap_eval(void* p)
{
	SchemeEval* self = (SchemeEval*) p;

	// Normally, neither of these are ever null.
	// But sometimes, a heavily loaded server can crash here.
	// Trying to figure out why ...
	OC_ASSERT(self, "c_wrap_eval got null pointer!");
	OC_ASSERT(self->pexpr, "c_wrap_eval got null expression!");

	self->do_eval(*(self->pexpr));
	return self;
}

/// Ad hoc hack to try to limit guile memory consumption, while still
/// providing decent performance.  The problem addressed here is that
/// guile can get piggy with the system RAM, happily gobbling up RAM
/// instead of garbage-collecting it.  Users have noticed. See github
/// issue #1116. This improves on the original pull request #1117.
/// Clue: bug report #1419 suggests the problem is related to the
/// do_async_output flag-- setting it to false avoids the blow-up.
/// Is there an i/o-related leak or weird reserve bug?
static void do_gc(void)
{
	static size_t prev_usage = 0;

	size_t curr_usage = getMemUsage();
	// Yes, this is 10MBytes. Which seems nutty. But it does the trick...
	if (10 * 1024 * 1024 < curr_usage - prev_usage)
	{
		prev_usage = curr_usage;
		scm_gc();
	}

#if 0
	static SCM since = scm_from_utf8_symbol("heap-allocated-since-gc");
	SCM stats = scm_gc_stats();
	size_t allo = scm_to_size_t(scm_assoc_ref(stats, since));

	int times = scm_to_int(scm_assoc_ref(stats, scm_from_utf8_symbol("gc-times")));
	printf("allo=%lu  mem=%lu time=%d\n", allo/1024, (getMemUsage() / (1024)), times);
	scm_gc();
	logger().info() << "Guile evaluated: " << expr;
	logger().info() << "Mem usage=" << (getMemUsage() / (1024*1024)) << "MB";
#endif
}

/**
 * do_eval -- evaluate a scheme expression string.
 * This implements the working guts of the shell-friendly evaluator.
 *
 * This method *must* be called in guile mode, in order for garbage
 * collection, etc. to work correctly!
 */
void SchemeEval::do_eval(const std::string &expr)
{
	per_thread_init();

	// Set global atomspace variable in the execution environment.
	AtomSpace* saved_as = NULL;
	if (atomspace)
	{
		saved_as = SchemeSmob::ss_get_env_as("do_eval");
		if (saved_as != atomspace)
			SchemeSmob::ss_set_env_as(atomspace);
		else
			saved_as = NULL;
	}

	_input_line += expr;

	redirect_output();
	_caught_error = false;
	_pending_input = false;
	error_msg.clear();
	set_captured_stack(SCM_BOOL_F);
	scm_gc_unprotect_object(_rc);
	SCM eval_str = scm_from_utf8_string(_input_line.c_str());
	_rc = scm_c_catch (SCM_BOOL_T,
	                      (scm_t_catch_body) scm_eval_string,
	                      (void *) eval_str,
	                      SchemeEval::catch_handler_wrapper, this,
	                      SchemeEval::preunwind_handler_wrapper, this);
	_rc = scm_gc_protect_object(_rc);

	restore_output();

	if (saved_as)
		SchemeSmob::ss_set_env_as(saved_as);

	if (++_gc_ctr%80 == 0) { do_gc(); _gc_ctr = 0; }

	_eval_done = true;
	_wait_done.notify_all();
}

void SchemeEval::begin_eval()
{
	_eval_done = false;
	_poll_done = false;
}

/// Read one end of a pipe. The other end of the pipe is attached to
/// guile's default output port.  We use standard posix to read, as
/// that will be faster than mucking with guile's one-char-at-a-time
/// API.  The below assumes that the pipe is non-blcoking; if it blocks,
/// then things might get wonky.  The below is also very simple; it does
/// not check for any standard unix errors, closed pipes, etc. I think
/// that simplicity is just fine, here.
std::string SchemeEval::poll_port()
{
	std::string rv;

	// drain_output() calls us, and not always in server mode.
	if (not _in_server) return rv;

#define BUFSZ 1000
	char buff[BUFSZ];
	while (1)
	{
		int nr = read(_pipeno, buff, BUFSZ-1);
		if (-1 == nr) return rv;
		buff[nr] = 0x0;
		rv += buff;
	}
	return rv;
}

/// Get output from evaluator, if any; block otherwise.
///
/// This method is meant to be called from a different thread than
/// the one that the scheme evaluator is running in.  It will try
/// to get any pending output, and will return that.  If there's no
/// pending output, then it will block until (1) there's output, or
/// (2) the evaluation has completed, in which case, it will return all
/// remaining output generated by the eval.
///
/// The entire goal of this function is to allow the use of the guile
/// 'display' can other stdio output calls from the interactive
/// environment, even if the actual scheme eval is very long-running.
/// Without this, what would happen is that all the output would get
/// buffered up, until the eval() finished, and then all this output
/// would get dumped.  For anything that runs more than a few seconds
/// this would be confusing and undesirable.  So, this method allows
/// some other thread to see if eval() is generating output, and, if
/// it is, to print it to stdout.
std::string SchemeEval::do_poll_result()
{
	per_thread_init();
	if (_poll_done) return "";

	if (not _eval_done)
	{
		// We don't have a real need to lock anything here; we're just
		// using this as a hack, so that the condition variable will
		// wake us up periodically.  The goal here is to block when
		// there's no output to be reported.
		//
		// Hmmm. I guess we could just block on reading the pipe...
		// unless the eval did not produce any output, in which case
		// I guess we could ... close and re-open the pipe? Somehow
		// unblock it?  I dunno. The below curently works, and I'm
		// loosing interest just right now.
		std::unique_lock<std::mutex> lck(_poll_mtx);
		while (not _eval_done)
		{
			_wait_done.wait_for(lck, std::chrono::milliseconds(300));
			std::string rv = poll_port();
			if (0 < rv.size()) return rv;
		}
	}
	// If we are here, then evaluation is done. Check the various
	// evalution result flags, etc.
	_poll_done = true;

	// Save the result of evaluation, and clear it. Recall that _rc is
	// typically set in a different thread.  We want it cleared before
	// we ever get here again, on later evals.
	SCM tmp_rc = _rc;
	scm_gc_unprotect_object(_rc);
	_rc = SCM_EOL;
	_rc = scm_gc_protect_object(_rc);

	/* An error is thrown if the input expression is incomplete,
	 * in which case the error handler sets the _pending_input flag
	 * to true. */
	if (_pending_input)
	{
		return "";
	}
	_pending_input = false;
	_input_line = "";

	if (_caught_error)
	{
		std::string rv = poll_port();

		char * str = scm_to_utf8_string(error_string);
		rv += str;
		free(str);
		set_error_string(SCM_EOL);
		set_captured_stack(SCM_BOOL_F);

		rv += "\n";
		return rv;
	}
	else
	{
		// First, we get the contents of the output port,
		// and pass that on.
		std::string rv = poll_port();

		// Next, we append the "interpreter" output
		rv += prt(tmp_rc);
		rv += "\n";
		scm_remember_upto_here_1(tmp_rc);
		return rv;
	}
	return "#<Error: Unreachable statement reached>";
}

/* ============================================================== */

/**
 * do_scm_eval -- evaluate a scheme expression
 *
 * Similar to do_eval(), with several important differences:
 * 1) The argument must be an SCM expression.
 * 2) The expression is evaluated by "evo"
 * 3) No shell-friendly string and output management is performed.
 * 4) Evaluation errors are logged to the log file.
 *
 * This method *must* be called in guile mode, in order for garbage
 * collection, etc. to work correctly!
 */
SCM SchemeEval::do_scm_eval(SCM sexpr, SCM (*evo)(void *))
{
	per_thread_init();

	// Set global atomspace variable in the execution environment.
	AtomSpace* saved_as = NULL;
	if (atomspace)
	{
		saved_as = SchemeSmob::ss_get_env_as("do_scm_eval");
		if (saved_as != atomspace)
			SchemeSmob::ss_set_env_as(atomspace);
		else
			saved_as = NULL;
	}

	// If we are running from the cogserver shell, capture all output
	if (_in_shell)
		redirect_output();

	_caught_error = false;
	error_msg.clear();
	set_captured_stack(SCM_BOOL_F);
	SCM rc = scm_c_catch (SCM_BOOL_T,
	                 evo, (void *) sexpr,
	                 SchemeEval::catch_handler_wrapper, this,
	                 SchemeEval::preunwind_handler_wrapper, this);

	_eval_done = true;

	// Restore the outport
	if (_in_shell)
		restore_output();

	if (saved_as)
		SchemeSmob::ss_set_env_as(saved_as);

	if (_caught_error)
	{
		char * str = scm_to_utf8_string(error_string);
		// Don't blank out the error string yet.... we need it later.
		// (probably because someone called cog-bind with an
		// ExecutionOutputLink in it with a bad scheme schema node.)
		// set_error_string(SCM_EOL);
		set_captured_stack(SCM_BOOL_F);

		// ?? Why are we discarding the output??
		drain_output();

		// Stick the guile stack trace into a string. Anyone who called
		// us is responsible for checking for an error, and handling
		// it as needed.
		error_msg = str;
		error_msg += "\n";

		free(str);
		return SCM_EOL;
	}

	// Get the contents of the output port, and log it
	if (_in_server and logger().isInfoEnabled())
	{
		std::string str(poll_port());
		if (0 < str.size())
		{
			logger().info("%s: Output: %s\n"
			              "Was generated by expr: %s\n",
			              __FUNCTION__, str.c_str(), prt(sexpr).c_str());
		}
	}

	// If we are in the cogservdr, but are anot in a shell context,
	// then truncate the output, because it will never ever be displayed.
	// (i.e. don't overflow the output buffers.) If we are in_shell,
	// then we are here probably because user typed something that
	// caused some ExecutionOutputLink to call some scheme snippet.
	// We do want to display that.
	if (_in_server and not _in_shell)
		drain_output();

	return rc;
}

/* ============================================================== */

SCM recast_scm_eval_string(void * expr)
{
	return scm_eval_string((SCM)expr);
}

/**
 * Evaluate a string containing a scheme expression, returning a Handle.
 * If an evaluation error occurs, an exception is thrown, and the stack
 * trace is logged to the log file.
 */
Handle SchemeEval::eval_h(const std::string &expr)
{
	// If we are recursing, then we already are in the guile
	// environment, and don't need to do any additional setup.
	// Just go.
	if (_in_eval) {
		// scm_from_utf8_string is lots faster than scm_from_locale_string
		SCM expr_str = scm_from_utf8_string(expr.c_str());
		SCM rc = do_scm_eval(expr_str, recast_scm_eval_string);
		return SchemeSmob::scm_to_handle(rc);
	}

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_lock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */

	pexpr = &expr;
	_in_eval = true;
	scm_with_guile(c_wrap_eval_h, this);
	_in_eval = false;

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_unlock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */

	// Convert evaluation errors into C++ exceptions.
	if (eval_error())
		throw RuntimeException(TRACE_INFO, "%s", error_msg.c_str());

	return hargs;
}

void * SchemeEval::c_wrap_eval_h(void * p)
{
	SchemeEval *self = (SchemeEval *) p;
	// scm_from_utf8_string is lots faster than scm_from_locale_string
	SCM expr_str = scm_from_utf8_string(self->pexpr->c_str());
	SCM rc = self->do_scm_eval(expr_str, recast_scm_eval_string);
	self->hargs = SchemeSmob::scm_to_handle(rc);
	return self;
}

/**
 * Evaluate a string containing a scheme expression, returning a TV.
 * If an evaluation error occurs, an exception is thrown, and the stack
 * trace is logged to the log file.
 */
TruthValuePtr SchemeEval::eval_tv(const std::string &expr)
{
	// If we are recursing, then we already are in the guile
	// environment, and don't need to do any additional setup.
	// Just go.
	if (_in_eval) {
		// scm_from_utf8_string is lots faster than scm_from_locale_string
		SCM expr_str = scm_from_utf8_string(expr.c_str());
		SCM rc = do_scm_eval(expr_str, recast_scm_eval_string);

		// Pass evaluation errors out of the wrapper.
		if (eval_error()) return TruthValue::NULL_TV();
		return SchemeSmob::to_tv(rc);
	}

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_lock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */

	pexpr = &expr;
	_in_eval = true;
	scm_with_guile(c_wrap_eval_tv, this);
	_in_eval = false;

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_unlock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */

	// Convert evaluation errors into C++ exceptions.
	if (eval_error())
		throw RuntimeException(TRACE_INFO, "%s", error_msg.c_str());

	return tvp;
}

void * SchemeEval::c_wrap_eval_tv(void * p)
{
	SchemeEval *self = (SchemeEval *) p;
	// scm_from_utf8_string is lots faster than scm_from_locale_string
	SCM expr_str = scm_from_utf8_string(self->pexpr->c_str());
	SCM rc = self->do_scm_eval(expr_str, recast_scm_eval_string);

	// Pass evaluation errors out of the wrapper.
	if (self->eval_error()) return self;

	self->tvp = SchemeSmob::to_tv(rc);
	return self;
}

/* ============================================================== */
/**
 * apply -- apply named function func to arguments in ListLink
 * It is assumed that varargs is a ListLink, containing a list of
 * atom handles. This list is unpacked, and then the function func
 * is applied to them. If the function returns an atom handle, then
 * this is returned. If the function does not return a handle, or if
 * an error ocurred during evaluation, then a C++ exception is thrown.
 */
Handle SchemeEval::apply(const std::string &func, Handle varargs)
{
	// If we are recursing, then we already are in the guile
	// environment, and don't need to do any additional setup.
	// Just go.
	if (_in_eval) {
		return do_apply(func, varargs);
	}

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_lock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */

	pexpr = &func;
	hargs = varargs;
	_in_eval = true;
	scm_with_guile(c_wrap_apply, this);
	_in_eval = false;

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_unlock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */
	if (eval_error())
		throw RuntimeException(TRACE_INFO, "%s", error_msg.c_str());

	return hargs;
}

void * SchemeEval::c_wrap_apply(void * p)
{
	SchemeEval *self = (SchemeEval *) p;
	self->hargs = self->do_apply(*self->pexpr, self->hargs);
	return self;
}

/**
 * do_apply -- apply named function func to arguments in ListLink
 * It is assumed that varargs is a ListLink, containing a list of
 * atom handles. This list is unpacked, and then the fuction func
 * is applied to them. If the function returns an atom handle, then
 * this is returned.
 */
Handle SchemeEval::do_apply(const std::string &func, Handle& varargs)
{
	// Apply the function to the args
	SCM sresult = do_apply_scm (func, varargs);

	// If the result is a handle, return the handle.
	return SchemeSmob::scm_to_handle(sresult);
}

static SCM thunk_scm_eval(void * expr)
{
	return scm_eval((SCM)expr, scm_interaction_environment());
}

/**
 * do_apply_scm -- apply named function func to arguments in ListLink
 * It is assumed that varargs is a ListLink, containing a list of
 * atom handles. This list is unpacked, and then the fuction func
 * is applied to them. The SCM value returned by the function is returned.
 */
SCM SchemeEval::do_apply_scm(const std::string& func, Handle& varargs )
{
	SCM sfunc = scm_from_utf8_symbol(func.c_str());
	SCM expr = SCM_EOL;

	// If there were args, pass the args to the function.
	const std::vector<Handle> &oset = atomspace->get_outgoing(varargs);

	// Iterate in reverse, because cons chains in reverse.
	size_t sz = oset.size();
	for (int i=sz-1; i>=0; i--)
	{
		SCM sh = SchemeSmob::handle_to_scm(oset[i]);
		expr = scm_cons(sh, expr);
	}
	expr = scm_cons(sfunc, expr);
	return do_scm_eval(expr, thunk_scm_eval);
}

/* ============================================================== */
/**
 * apply_tv -- apply named function func to arguments in ListLink.
 * Return an OpenCog TruthValuePtr.
 * It is assumed that varargs is a ListLink, containing a list of
 * atom handles. This list is unpacked, and then the fuction func
 * is applied to them. The function is presumed to return pointer
 * to a TruthValue object.
 */
TruthValuePtr SchemeEval::apply_tv(const std::string &func, Handle varargs)
{
	// If we are recursing, then we already are in the guile
	// environment, and don't need to do any additional setup.
	// Just go.
	if (_in_eval) {
		SCM tv_smob = do_apply_scm(func, varargs);
		if (eval_error())
			return TruthValue::NULL_TV();
		return SchemeSmob::to_tv(tv_smob);
	}

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_lock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */

	pexpr = &func;
	hargs = varargs;
	_in_eval = true;
	scm_with_guile(c_wrap_apply_tv, this);
	_in_eval = false;

#ifdef WORK_AROUND_GUILE_THREADING_BUG
	thread_unlock();
#endif /* WORK_AROUND_GUILE_THREADING_BUG */
	if (eval_error())
		throw RuntimeException(TRACE_INFO, "%s", error_msg.c_str());

	// We do not want this->tvp to point at anything after we return.
	// This is so that we do not hold a long-term reference to the TV.
	TruthValuePtr rtv;
	swap(rtv, tvp);
	return rtv;
}

void * SchemeEval::c_wrap_apply_tv(void * p)
{
	SchemeEval *self = (SchemeEval *) p;
	SCM tv_smob = self->do_apply_scm(*self->pexpr, self->hargs);
	if (self->eval_error()) return self;
	self->tvp = SchemeSmob::to_tv(tv_smob);
	return self;
}

/* ============================================================== */

// A pool of scheme evaluators, sitting hot and ready to go.
// This is used to implement get_evaluator(), below.  The only
// reason this is done with a pool, instead of simply new() and
// delete() is because calling delete() from TLS conflicts with
// the guile garbage collector, when the thread is destroyed. See
// the note below.
static concurrent_stack<SchemeEval*> pool;
static std::mutex pool_mtx;

static SchemeEval* get_from_pool(void)
{
	std::lock_guard<std::mutex> lock(pool_mtx);
	SchemeEval* ev = NULL;
	if (pool.try_pop(ev)) return ev;
	return new SchemeEval();
}

static void return_to_pool(SchemeEval* ev)
{
	std::lock_guard<std::mutex> lock(pool_mtx);
	pool.push(ev);
}

/// Return evaluator, for this thread and atomspace combination.
/// If called with NULL, it will use the current atomspace for
/// this thread.
///
/// Use thread-local storage (TLS) in order to avoid repeatedly
/// creating and destroying the evaluator.
///
/// This will throw an error if used recursively.  Viz, if the
/// evaluator evaluates something that causes another evaluator
/// to be needed for this thread (e.g. an ExecutionOutputLink),
/// then this very same evaluator would be re-entered, corrupting
/// its own internal state.  If that happened, the result would be
/// a hard-to-find & fix bug. So instead, we throw.
SchemeEval* SchemeEval::get_evaluator(AtomSpace* as)
{
	static thread_local std::map<AtomSpace*,SchemeEval*> issued;

	// The eval_dtor runs when this thread is destroyed.
	class eval_dtor {
		public:
		~eval_dtor() {
			for (auto ev : issued)
			{
				SchemeEval* evaluator = ev.second;

				// It would have been easier to just call delete evaluator
				// instead of return_to_pool.  Unfortunately, the delete
				// won't work, because the STL destructor has already run
				// the guile GC at this point, for this thread, and so
				// calling delete will lead to a crash in c_wrap_finish().
				// It would be nice if we got called before guile did, but
				// there is no way in TLS to control execution order...
				evaluator->atomspace = NULL;
				return_to_pool(evaluator);
			}
		}
	};
	static thread_local eval_dtor killer;

	auto ev = issued.find(as);
	if (ev != issued.end())
		return ev->second;

	SchemeEval* evaluator = get_from_pool();
	evaluator->atomspace = as;
	issued[as] = evaluator;
	return evaluator;

#if 0
	if (evaluator->recursing())
		throw RuntimeException(TRACE_INFO,
			"Evaluator thread singleton used recursively!");
#endif

}

/* ============================================================== */

void* SchemeEval::c_wrap_set_atomspace(void * vas)
{
	AtomSpace* as = (AtomSpace*) vas;
	SchemeSmob::ss_set_env_as(as);
	return vas;
}

/**
 * Set the current atomspace for this thread.  From this point on, all
 * scheme code executing in this thread will use this atomspace (unless
 * it is changed in the course of execution...)
 */
void SchemeEval::set_scheme_as(AtomSpace* as)
{
	scm_with_guile(c_wrap_set_atomspace, as);
}

void SchemeEval::init_scheme(void)
{
	// XXX FIXME only a subset is needed.
	SchemeEval sch;
}

#endif

/* ===================== END OF FILE ============================ */
