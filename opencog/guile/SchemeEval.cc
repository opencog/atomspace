/**
 * SchemeEval.cc
 *
 * Scheme evaluator.  Given strings in scheme, evaluates them with
 * the appropriate Atomspace, etc.
 *
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <atomic>

#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <termios.h>

#include <cstddef>
#include <libguile.h>
#include <libguile/backtrace.h>
#include <libguile/debug.h>

#include <opencog/util/platform.h>
#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>
#include <opencog/util/platform.h>
#include <opencog/atoms/truthvalue/TruthValue.h>

#include "SchemeEval.h"
#include "SchemePrimitive.h"
#include "SchemeSmob.h"

using namespace opencog;

static std::mutex init_mtx;

/**
 * This init is called once for every time that this class
 * is instantiated -- i.e. it is a per-instance initializer.
 */
void SchemeEval::init(void)
{
#define WORK_AROUND_BUG_25238
#ifdef WORK_AROUND_BUG_25238
	// See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25238
	std::lock_guard<std::mutex> lck(init_mtx);
#endif // WORK_AROUND_BUG_25238

#define WORK_AROUND_GUILE_UTF8_BUGS
#ifdef WORK_AROUND_GUILE_UTF8_BUGS
	// Arghhh!  Avoid ongoing utf8 fruitcake nutiness in guile-2.0
	scm_c_eval_string ("(setlocale LC_ALL \"\")\n");
	// Force iso standard numeric formatting
	scm_c_eval_string ("(setlocale LC_NUMERIC \"C\")\n");
#endif // WORK_AROUND_GUILE_UTF8_BUGS

	SchemeSmob::init();
	PrimitiveEnviron::init();

	_in_server = false;
	_in_redirect = 0;
	_in_shell = false;
	_in_eval = false;
	_eval_thread = SCM_EOL;

	// User error and crash management
	_scm_error_string = SCM_EOL;
	_scm_error_string = scm_gc_protect_object(_scm_error_string);

	_captured_stack = SCM_BOOL_F;
	_captured_stack = scm_gc_protect_object(_captured_stack);

	_pexpr = NULL;
	_eval_done = true;
	_poll_done = true;

	_rc = SCM_EOL;
	_rc = scm_gc_protect_object(_rc);

	// We expect one evaluator per thread, so set that up now.
	// More complicated possibilities are too hard to deal with:
	// evaluated expressions might themselves be setting the
	// AtomSpace of the thread.
	if (_atomspace)
		SchemeSmob::ss_set_env_as(_atomspace);
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
	// XXX FIXME This lock is not needed, because in guile-2.2,
	// at least, every thread has its own output port, and so its
	// impossible for two different threads to compete to set the
	// same outport.  Not too sure about guile-2.0, though... so
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
	// Make the port be unbuffered -- we want bytes right away!
#if (SCM_MAJOR_VERSION==2 && SCM_MINOR_VERSION==1 && SCM_MICRO_VERSION>=3) \
    || (SCM_MAJOR_VERSION==2 && SCM_MINOR_VERSION>1) \
    || (SCM_MAJOR_VERSION>2)

	// As of version 2.1.3, the API changed in an incompatible way...
	static SCM no_buffering = scm_from_utf8_symbol("none");
	scm_setvbuf(_outport, no_buffering, SCM_UNDEFINED);
#else
	scm_setvbuf(_outport, scm_from_int(_IONBF), SCM_UNDEFINED);
#endif

	// We want non-blocking reads.
	int flags = fcntl(_pipeno, F_GETFL, 0);
	if (flags < 0) flags = 0;
	fcntl(_pipeno, F_SETFL, flags | O_NONBLOCK);
}

/// Use the async I/O mechanism, if we are in the cogserver.
///
/// Note, by the way, that Guile implements the current port as a fluid
/// on each thread. So the save and restore implemented here gives us
/// exactly the right per-thread semantics.
void SchemeEval::redirect_output(void)
{
	_in_redirect++;
	if (1 < _in_redirect) return;
	capture_port();

	// Output ports for side-effects.
	_saved_outport = scm_current_output_port();
	_saved_outport = scm_gc_protect_object(_saved_outport);

	scm_set_current_output_port(_outport);

	_eval_thread = scm_current_thread();
}

void SchemeEval::restore_output(void)
{
	_in_redirect --;
	if (0 < _in_redirect) return;

	// Restore the previous outport (if its still alive)
	if (scm_is_false(scm_port_closed_p(_saved_outport)))
		scm_set_current_output_port(_saved_outport);
	scm_gc_unprotect_object(_saved_outport);

	_eval_thread = SCM_EOL;
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
	// Unset the AtomSpace for this thread.
	SchemeSmob::ss_set_env_as(nullptr);

	std::lock_guard<std::mutex> lck(init_mtx);
	scm_gc_unprotect_object(_rc);

	// If we had once set up the async I/O, the release it.
	if (_in_server)
	{
		scm_close_port(_outport);
		scm_gc_unprotect_object(_outport);

		scm_close_port(_pipe);
		scm_gc_unprotect_object(_pipe);
	}

	scm_gc_unprotect_object(_scm_error_string);
	scm_gc_unprotect_object(_captured_stack);
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
	SCM oldstack = _captured_stack;
	_captured_stack = scm_gc_protect_object(newstack);
	scm_gc_unprotect_object(oldstack);
}

void SchemeEval::set_error_string(SCM newerror)
{
	SCM olderror = _scm_error_string;
	_scm_error_string = scm_gc_protect_object(newerror);
	scm_gc_unprotect_object(olderror);
}

static thread_local bool thread_is_inited = false;

// This will throw an exception, when it is called.  It is used
// to interrupt infinite loops or long-running processes, when the
// user hits control-C at a telnet prompt.
static SCM throw_except(void)
{
	scm_throw(
		scm_from_utf8_symbol("user-interrupt"),
		scm_list_2(
			scm_from_utf8_string("SchemeEval::interrupt"),
			scm_from_utf8_string("User interrupt from keyboard")));

	/* not reached */ return SCM_EOL;
}

static SCM throw_thunk = SCM_EOL;

void* c_wrap_init_only_once(void* p)
{
	throw_thunk = scm_c_make_gsubr("cog-throw-user-interrupt",
		0, 0, 0, ((scm_t_subr) throw_except));
	return nullptr;
}

// Cheap, simple semaphore.
static volatile bool done_with_init = false;

static void immortal_thread(void)
{
	scm_with_guile(c_wrap_init_only_once, NULL);
	set_thread_name("atoms:immortal");

	// Tell compiler to set flag dead-last, after above has executed.
	asm volatile("": : :"memory");
	done_with_init = true;

	// sleep forever-and-ever.
	while (true) { pause(); }
}

// Create an immortal thread. Force all other threads to spin, until
// that immortal thread has been created and is done starting.
//
// The first time that guile is initialized, it MUST be done in some
// thread that will never-ever exit. If this thread exits, the bdwgc
// will not ever find out about it, and this will scramble it's
// state.  This is documented in two related bugs:
// https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26711  and
// https://github.com/opencog/atomspace/issues/1054
static void init_only_once(void)
{
	static std::atomic_flag call_once_flag = ATOMIC_FLAG_INIT;

	if (done_with_init) return;

	// The call_once flag just ensures that we make only one
	// immortal thread. Same thing as
	// std::call_once(flag, []() { new std::thread(...); });
	if (not call_once_flag.test_and_set())
		new std::thread(immortal_thread);

	// Spin. This is the semaphore wait() emulation.
	while (not done_with_init) { usleep(1000); }
}

SchemeEval::SchemeEval(AtomSpace* as)
{
	init_only_once();

	// If it is coming from the pool, the as will be null.
	if (as)
		_atomspace = AtomSpaceCast(as->shared_from_this());
	else
		_atomspace = nullptr;

	scm_with_guile(c_wrap_init, this);
}

SchemeEval::SchemeEval(AtomSpacePtr& asp)
{
	init_only_once();
	_atomspace = asp;

	scm_with_guile(c_wrap_init, this);
}

/* This should be called once for every new thread. */
void SchemeEval::per_thread_init(void)
{
	/* Avoid more than one call per thread. */
	if (thread_is_inited) return;
	thread_is_inited = true;

#ifdef WORK_AROUND_GUILE_UTF8_BUGS
	// Arghhh!  Avoid ongoing utf8 fruitcake nutiness in guile-2.0
	scm_c_eval_string ("(setlocale LC_ALL \"\")\n");
	// Force iso standard numeric formatting
	scm_c_eval_string ("(setlocale LC_NUMERIC \"C\")\n");
#endif // WORK_AROUND_GUILE_UTF8_BUGS
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

	// If the user types (quit) or (exit) at the cogserver shell, we will
	// end up here. That's because (quit) (exit) and (exit 42) all get
	// converted into (throw 'quit (list 42)). We have two choices:
	// Rethrow this to the main guile shell, or just exit(). Now, in
	// guile-3.0, the (throw 'quit) just turns into an exit(), so it
	// mostly doesn't matter what we do (although guile allows (quit #t)
	// which means exit(0) and I don't want to futz with this trivia.)
	// However, we do one important thing: the guile repl loop does
	// mess with the termios settings and mangles the shell. So we
	// do a minimalist `stty sane` here.
	if (0 == strcmp(restr, "quit"))
	{
		logger().info("[SchemeEval]: exit\n");

		struct termios tio;
		tcgetattr (STDIN_FILENO, &tio);

		tio.c_iflag = tio.c_iflag | ICRNL;
		tio.c_iflag = tio.c_iflag | IXON;
		tio.c_iflag = tio.c_iflag | IUTF8;

		tio.c_lflag = tio.c_lflag | ISIG;
		tio.c_lflag = tio.c_lflag | ICANON;
		tio.c_lflag = tio.c_lflag | ECHO;

		tcsetattr(STDIN_FILENO, TCSADRAIN, &tio);

		scm_throw(scm_from_utf8_symbol("quit"), throw_args);

		// The above should not return.  It should have done this:
		if (SCM_EOL == throw_args) exit(0);
		exit(scm_to_int(scm_car(throw_args)));

		// r7rs allows (quit #t) which means exit(0) and (quit #f)
		// which means exit(1) but we are not going to futz with this.
		// Because the throw above should not return.
		exit(1);
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

		if (scm_is_true (_captured_stack))
		{
			SCM highlights;

			if (scm_is_eq (tag, scm_arg_type_key) ||
				scm_is_eq (tag, scm_out_of_range_key))
				highlights = rest;
			else
				highlights = SCM_EOL;

			scm_puts ("Backtrace:\n", port);
			scm_display_backtrace_with_highlights (_captured_stack, port,
			                                       SCM_BOOL_F, SCM_BOOL_F,
			                                       highlights);
			scm_newline (port);

			if (SCM_STACK_LENGTH(_captured_stack))
				set_captured_stack(scm_stack_ref (_captured_stack, SCM_INUM0));

			scm_display_error(_captured_stack, port, subr, message, parts, rest);
		}
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
 *    been enough input received to evaluate without error.
 * 2) It catches errors, and prints the catch in a reasonably nicely
 *    formatted way.
 * 3) It converts any returned scheme expressions to a string, for easy
 *    printing.
 * 4) It concatenates any data sent to the scheme output port (e.g.
 *    printed output from the scheme (display) function) to the returned
 *    string.
 *
 * An "unforgiving" evaluator, with none of these amenities, can be
 * found in eval_v(), below.
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

	_pexpr = &expr;
	_in_shell = true;
	_in_eval = true;
	scm_with_guile(c_wrap_eval, this);
	_in_eval = false;
	_in_shell = false;
}

void* SchemeEval::c_wrap_poll(void* p)
{
	SchemeEval* self = (SchemeEval*) p;
	self->_answer = self->do_poll_result();
	return p;
}

std::string SchemeEval::poll_result()
{
	scm_with_guile(c_wrap_poll, this);
	return _answer;
}

void* SchemeEval::c_wrap_eval(void* p)
{
	SchemeEval* self = (SchemeEval*) p;

	// Normally, neither of these are ever null.
	// But sometimes, a heavily loaded server can crash here.
	// Trying to figure out why ...
	OC_ASSERT(self, "c_wrap_eval got null pointer!");
	OC_ASSERT(self->_pexpr, "c_wrap_eval got null expression!");

	// There might be multiple evaluators running in this thread.
	// Set the thread-space unconditionally.
	if (self->_atomspace)
		SchemeSmob::ss_set_env_as(self->_atomspace);

	self->do_eval(*(self->_pexpr));
	return self;
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

	_input_line += expr;

	redirect_output();
	_caught_error = false;
	_pending_input = false;
	_error_msg.clear();
	set_captured_stack(SCM_BOOL_F);

	// When invoked from the cogserver shell, it can happen that
	// telnet control characters sneak through. These will not be
	// valid utf8 strings, and scm_from_utf8_string will throw.
	// Avoid some bad behavior by catching and cleaning up.
	SCM eval_str = scm_internal_catch(SCM_BOOL_T,
	                      (scm_t_catch_body) scm_from_utf8_string,
	                      (void *) _input_line.c_str(),
	                      SchemeEval::catch_handler_wrapper, this);

	if (not _caught_error)
	{
		SCM rc = scm_c_catch (SCM_BOOL_T,
	                      (scm_t_catch_body) scm_eval_string,
	                      (void *) eval_str,
	                      SchemeEval::catch_handler_wrapper, this,
	                      SchemeEval::preunwind_handler_wrapper, this);
		save_rc(rc);
	}
	restore_output();

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

	// int pipe_size;
	// ioctl(_pipeno, FIONREAD, &pipe_size);
	// printf("Size of the pipe (bytes): %d\n", pipe_size);

#define BUFSZ 65000
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

/// Set the _rc member in an pseudo-atomic fashion.
///
/// This should never-ever need to actually be done atomically, so if
/// you witness a crash in this code, then there is a bug in the code
/// that is using this instance!  There have been bugs in the past, in
/// the cogserver SchemeShell.
void SchemeEval::save_rc(SCM rc)
{
	scm_gc_unprotect_object(_rc);
	_rc = rc;
	_rc = scm_gc_protect_object(_rc);
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
		// unblock it?  I dunno. The below currently works, and I'm
		// losing interest just right now.
		std::unique_lock<std::mutex> lck(_poll_mtx);
		while (not _eval_done)
		{
			_wait_done.wait_for(lck, std::chrono::milliseconds(300));
			std::string rv = poll_port();
			if (0 < rv.size()) return rv;
		}
	}

	// Bugs in the scheme shell can trigger this assert!
	// This can happen if the evaluator is mis-used.
	OC_ASSERT(_poll_done == false);

	// Save the result of evaluation, and clear it. Recall that _rc is
	// typically set in a different thread.  We want it cleared before
	// we ever get here again, on later evals.
	SCM tmp_rc = _rc;
	save_rc(SCM_EOL);

	// If we are here, then evaluation is done. Check the various
	// evaluation result flags, etc.
	_poll_done = true;

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
		_error_string = poll_port();

		char * str = scm_to_utf8_string(_scm_error_string);
		_error_string += str;
		free(str);
		set_error_string(SCM_EOL);
		set_captured_stack(SCM_BOOL_F);

		_error_string += "\n";
		return _error_string;
	}

	// First, we get the contents of the output port,
	// and pass that on.
	std::string rv = poll_port();

	// Next, we append the "interpreter" output
	rv += prt(tmp_rc);
	rv += "\n";
	scm_remember_upto_here_1(tmp_rc);
	return rv;
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

	// If we are running from the cogserver shell, capture all output
	if (_in_shell)
		redirect_output();

	_caught_error = false;
	_error_msg.clear();
	set_captured_stack(SCM_BOOL_F);
	SCM rc = scm_c_catch (SCM_BOOL_T,
	                 evo, (void *) sexpr,
	                 SchemeEval::catch_handler_wrapper, this,
	                 SchemeEval::preunwind_handler_wrapper, this);

	// Restore the outport
	if (_in_shell)
		restore_output();

	if (_caught_error)
	{
		char * str = scm_to_utf8_string(_scm_error_string);
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
		_error_msg = str;
		_error_msg += "\n";

		free(str);
		return SCM_EOL;
	}

	// Get the contents of the output port, and log it
	if (_in_server and logger().is_info_enabled())
	{
		std::string str(poll_port());
		if (0 < str.size())
		{
			logger().info("%s: Output: %s\n"
			              "Was generated by expr: %s\n",
			              __FUNCTION__, str.c_str(), prt(sexpr).c_str());
		}
	}

	// If we are in the cogserver, but are not in a shell context,
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

/**
 * interrupt() - convert user's control-C at keyboard into exception.
 *
 * Calling this will interrupt whatever guile processing this evaluator
 * is doing -- it calls the guile routine `cog-throw-user-interrupt`
 * which calls the `SchemeEval::throw_except()` method.  The resulting
 * exception should halt whatever might be running in the _eval_thread
 * associated with this evaluator.
 */
void SchemeEval::interrupt(void)
{
	if (SCM_EOL == _eval_thread) return;
	scm_with_guile(c_wrap_interrupt, this);
}

void * SchemeEval::c_wrap_interrupt(void* p)
{
	SchemeEval *self = (SchemeEval *) p;
	SCM thr = self->_eval_thread;
	if (SCM_EOL == thr) return self;

	scm_system_async_mark_for_thread(throw_thunk, thr);

	return self;
}

/* ============================================================== */

SCM recast_scm_eval_string(void * expr)
{
	return scm_eval_string((SCM)expr);
}

void * SchemeEval::c_wrap_eval_v(void * p)
{
	SchemeEval *self = (SchemeEval *) p;

	// Set per-thread atomspace variable in the execution environment.
	if (self->_atomspace)
		SchemeSmob::ss_set_env_as(self->_atomspace);

	// scm_from_utf8_string is lots faster than scm_from_locale_string
	SCM expr_str = scm_from_utf8_string(self->_pexpr->c_str());
	SCM rc = self->do_scm_eval(expr_str, recast_scm_eval_string);

	// Pass evaluation errors out of the wrapper.
	if (self->eval_error()) return self;

	self->_retval = SchemeSmob::scm_to_protom(rc);
	return self;
}


/**
 * Evaluate a string containing a scheme expression, returning a
 * ProtoAtom (Handle, TruthValue or Value).  If an evaluation error
 * occurs, an exception is thrown, and the stack trace is logged to
 * the log file.
 */
ValuePtr SchemeEval::eval_v(const std::string &expr)
{
	// If we are recursing, then we already are in the guile
	// environment, and don't need to do any additional setup.
	// Just go.
	// In this case, "recursing" means that guile called something
	// that triggered a GroundedPredicate/SchemaNode that called
	// guile, again, in this very same thread.  Another possibility
	// is that some scheme code called cog-execute! explicitly.
	if (_in_eval) {
		// scm_from_utf8_string is lots faster than scm_from_locale_string
		SCM expr_str = scm_from_utf8_string(expr.c_str());
		// An alternative here would be to evaluate the string directly,
		// so that any exceptions thrown get passed right on up the stack.
		// I think this is the right thing to do; but I'm a bit confused.
		// The alternative would be this:
		// SCM rc = scm_eval_string(expr_str);
		// However, I suspect that might actually result in the exception
		// being hidden away.  So lets be conservative, and throw.
		SCM rc = do_scm_eval(expr_str, recast_scm_eval_string);
		if (eval_error())
			throw RuntimeException(TRACE_INFO, "%s", _error_msg.c_str());
		return SchemeSmob::scm_to_protom(rc);
	}

	_pexpr = &expr;
	_in_eval = true;
	scm_with_guile(c_wrap_eval_v, this);
	_in_eval = false;

	// Convert evaluation errors into C++ exceptions.
	if (eval_error())
		throw RuntimeException(TRACE_INFO, "%s", _error_msg.c_str());

	// We do not want this->_retval to point at anything after we return.
	// This is so that we do not hold a long-term reference to the TV.
	ValuePtr rv;
	swap(rv, _retval);
	return rv;
}

/**
 * Evaluate a string containing a scheme expression, returning an
 * AtomSpace.
 * If an evaluation error occurs, an exception is thrown, and the stack
 * trace is logged to the log file.
 */
AtomSpacePtr SchemeEval::eval_as(const std::string &expr)
{
	// If we are recursing, then we already are in the guile
	// environment, and don't need to do any additional setup.
	// Just go.
	if (_in_eval) {
		// scm_from_utf8_string is lots faster than scm_from_locale_string
		SCM expr_str = scm_from_utf8_string(expr.c_str());
		SCM rc = do_scm_eval(expr_str, recast_scm_eval_string);

		// Pass evaluation errors out of the wrapper.
		if (eval_error()) return nullptr;
		return SchemeSmob::ss_to_atomspace(rc);
	}

	_pexpr = &expr;
	_in_eval = true;
	scm_with_guile(c_wrap_eval_as, this);
	_in_eval = false;

	// Convert evaluation errors into C++ exceptions.
	if (eval_error())
		throw RuntimeException(TRACE_INFO, "%s", _error_msg.c_str());

	return _retas;
}

void * SchemeEval::c_wrap_eval_as(void * p)
{
	SchemeEval *self = (SchemeEval *) p;

	// Set per-thread atomspace variable in the execution environment.
	if (self->_atomspace)
		SchemeSmob::ss_set_env_as(self->_atomspace);

	// scm_from_utf8_string is lots faster than scm_from_locale_string
	SCM expr_str = scm_from_utf8_string(self->_pexpr->c_str());
	SCM rc = self->do_scm_eval(expr_str, recast_scm_eval_string);

	// Pass evaluation errors out of the wrapper.
	if (self->eval_error()) return self;

	self->_retas = SchemeSmob::ss_to_atomspace(rc);
	return self;
}

/* ============================================================== */
/* ============================================================== */

static SCM thunk_scm_eval(void * expr)
{
	return scm_eval((SCM)expr, scm_interaction_environment());
}

/**
 * do_apply_scm -- apply named function func to arguments in ListLink
 * It is assumed that varargs is a ListLink, containing a list of
 * atom handles. This list is unpacked, and then the function func
 * is applied to them. The SCM value returned by the function is returned.
 */
SCM SchemeEval::do_apply_scm(const std::string& func, const Handle& varargs )
{
	SCM sfunc = scm_from_utf8_symbol(func.c_str());
	SCM expr = SCM_EOL;

	// If there were args, pass the args to the function.
	if (varargs)
	{
		// If varargs is a ListLink, its elements are passed to the
		// function, otherwise the single argument is passed.
		HandleSeq single_arg{varargs};
		const HandleSeq &oset = varargs->get_type() == LIST_LINK ?
			varargs->getOutgoingSet() : single_arg;

		// Iterate in reverse, because cons chains in reverse.
		size_t sz = oset.size();
		for (size_t i=sz; i>0; i--)
		{
			SCM sh = SchemeSmob::handle_to_scm(oset[i-1]);
			expr = scm_cons(sh, expr);
		}
	}
	expr = scm_cons(sfunc, expr);

	// Set per-thread atomspace variable in the execution environment.
	if (_atomspace)
		SchemeSmob::ss_set_env_as(_atomspace);

	// TODO: it would be nice to pass exceptions on through, but
	// this currently breaks unit tests.
	// if (_in_eval)
	//    return scm_eval(expr, scm_interaction_environment());
	return do_scm_eval(expr, thunk_scm_eval);
}

/* ============================================================== */
/**
 * apply_v -- apply named function func to arguments in ListLink.
 * Return an OpenCog ValuePtr (Handle, TruthValue or Value).
 * It is assumed that varargs is a ListLink, containing a list of
 * atom handles. This list is unpacked, and then the function func
 * is applied to them. The function is presumed to return pointer
 * to a ProtoAtom [now renamed Value] object. If the function does
 * not return a ProtoAtom, or if n error occurred during evaluation,
 * then a C++ exception is thrown.
 */
ValuePtr SchemeEval::apply_v(const std::string &func, Handle varargs)
{
	// If we are recursing, then we already are in the guile
	// environment, and don't need to do any additional setup.
	// Just go.
	if (_in_eval) {
		SCM smob = do_apply_scm(func, varargs);
		if (eval_error())
		{
			// Rethrow.  It would be better to just allow exceptions
			// to pass on through, but thus breaks some unit tests.
			// XXX FIXME -- idealy we should avoid catch-and-rethrow.
			// At any rate, we must not return a TV of any sort, here.
			throw RuntimeException(TRACE_INFO, "%s", _error_msg.c_str());
		}
		return SchemeSmob::scm_to_protom(smob);
	}

	_pexpr = &func;
	_hargs = varargs;
	_in_eval = true;
	scm_with_guile(c_wrap_apply_v, this);
	_in_eval = false;
	_hargs = nullptr;

	if (eval_error())
		throw RuntimeException(TRACE_INFO, "Unable to apply `%s` to\n%s\n%s",
			func.c_str(),
			(nullptr == varargs) ? "(nullptr)" : varargs->to_string().c_str(),
			_error_msg.c_str());

	// We do not want this->_retval to point at anything after we return.
	// This is so that we do not hold a long-term reference to the TV.
	ValuePtr rv;
	swap(rv, _retval);
	return rv;
}

void * SchemeEval::c_wrap_apply_v(void * p)
{
	SchemeEval *self = (SchemeEval *) p;
	SCM smob = self->do_apply_scm(*self->_pexpr, self->_hargs);
	if (self->eval_error()) return self;
	self->_retval = SchemeSmob::scm_to_protom(smob);
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
	ev->clear_pending();
	std::lock_guard<std::mutex> lock(pool_mtx);

	// try..catch is needed during library exit; the stack may
	// already be gone. So just ignore the resulting exception.
	// This should only happen during finalization.
	try {
		pool.push(ev);
	}
	catch (const concurrent_stack<SchemeEval*>::Canceled&) {}
}

/// Return evaluator, for this thread and atomspace combination.
/// If called with NULL, it will use the current atomspace for
/// this thread.
///
/// Use thread-local storage (TLS) in order to avoid repeatedly
/// creating and destroying the evaluator.
///
SchemeEval* SchemeEval::get_evaluator(const AtomSpacePtr& asp)
{
	static thread_local std::map<AtomSpacePtr,SchemeEval*> issued;

	// The eval_dtor runs when this thread is destroyed.
	class eval_dtor {
		public:
		~eval_dtor() {
			for (auto ev : issued)
			{
				SchemeEval* evaluator = ev.second;

				// It would have been easier to just call delete evaluator
				// instead of return_to_pool.  Unfortunately, the delete
				// won't work, because the TLS thread destructor has already
				// run the guile GC at this point, for this thread, and so
				// calling delete will lead to a crash in c_wrap_finish().
				// It would be nice if we got called before guile did, but
				// there is no way in TLS to control execution order...
				evaluator->_atomspace = nullptr;
				return_to_pool(evaluator);
			}
		}
	};
	static thread_local eval_dtor killer;

	auto ev = issued.find(asp);
	if (ev != issued.end())
		return ev->second;

	SchemeEval* evaluator = get_from_pool();
	evaluator->_atomspace = asp;
	issued[asp] = evaluator;

	return evaluator;
}

SchemeEval* SchemeEval::get_evaluator(AtomSpace* as)
{
	// A null AtomSpace is passed from the cython initialization
	// code. That code scramble to create an AtomSpace, after
	// guile is initialized.
	static AtomSpacePtr nullasp;
	if (nullptr == as) return get_evaluator(nullasp);

	const AtomSpacePtr& asp = AtomSpaceCast(as->shared_from_this());
	return get_evaluator(asp);
}

/* ============================================================== */

void* SchemeEval::c_wrap_get_atomspace(void * p)
{
	SchemeEval *self = (SchemeEval *) p;
	self->_retas = SchemeSmob::ss_get_env_as("get_scheme_as");
	return self;
}

AtomSpacePtr SchemeEval::get_scheme_as(void)
{
	scm_with_guile(c_wrap_get_atomspace, this);
	return _retas;
}

void* SchemeEval::c_wrap_set_atomspace(void * vas)
{
	if (nullptr == vas) return vas;
	AtomSpace* as = (AtomSpace*) vas;
	const AtomSpacePtr& asp = AtomSpaceCast(as->shared_from_this());
	SchemeSmob::ss_set_env_as(asp);
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

void SchemeEval::set_scheme_as(const AtomSpacePtr& as)
{
	scm_with_guile(c_wrap_set_atomspace, as.get());
}

void SchemeEval::init_scheme(void)
{
	// XXX FIXME only a subset is needed.
	SchemeEval sch;
}

extern "C" {
// Thin wrapper for easy dlopen/dlsym dynamic loading
opencog::SchemeEval* get_scheme_evaluator(opencog::AtomSpace* as)
{
	return opencog::SchemeEval::get_evaluator(as);
}

};


/* ===================== END OF FILE ============================ */
