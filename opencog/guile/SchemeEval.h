/*
 * SchemeEval.h
 *
 * Scheme expression evaluator for OpenCog
 * Copyright (c) 2008, 2014 Linas Vepstas <linas@linas.org>
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

#ifndef OPENCOG_SCHEME_EVAL_H
#define OPENCOG_SCHEME_EVAL_H
#ifdef HAVE_GUILE

#include <condition_variable>
#include <mutex>
#include <string>
#include <sstream>
#include <cstddef>
#include <libguile.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/eval/GenericEval.h>
#include <opencog/atoms/truthvalue/TruthValue.h>

namespace opencog {
/** \addtogroup grp_smob
 *  @{
 *
 * The evaluator provides two modes of usage: synchronous and
 * asynchronous.  The synchronous API is provided by the following
 * methods: eval(), eval_h(), apply(), and apply_generic(). The last
 * three are special-purpose wrappers around the first, returning
 * handles, and applying functions to argument lists.
 *
 * The eval() method returns a string, holding any output that was
 * printed during evaluation. e.g. output printed using the scheme
 * 'display' function. If the code to be evaluated is long-running,
 * then nothing can be returned until the evaluation completes. This
 * presents a problem when the thing to be evaluated is perhaps an
 * infinite loop.
 *
 * Thus, the asynchronous interface is provided. This is implemented
 * with three methods: begin_eval(), eval_expr(), and poll_result().
 * If the expression that needs to be evaluated is long-running, or
 * even if it is an infinite loop, yet which periodically prints, then
 * it can be run in one thread, and the print output can be collected in
 * another thread, with poll_result().  The poll_result() method will
 * block until there is printed output. It can be called repeatedly.
 * When evaluation is completed, it will return any pending printed
 * output, and subsequent calls will return the empty string
 * immediately, without any further blocking.  Be sure to call
 * begin_eval() first, to set things up.
 *
 * The synchronous implementation is built on top of the async one,
 * and runs entirely within the same thread; see the code below; it
 * should show that:
 *
 *      std::string eval(const std::string& expr)
 *         { begin_eval(); eval_expr(expr); return poll_result(); }
 *
 */

class AtomSpace;

class SchemeEval : public GenericEval
{
	private:
		// Initialization stuff
		void init(void);
		static void * c_wrap_init(void *);
		void per_thread_init(void);

		// Destructor stuff
		void finish(void);
		static void * c_wrap_finish(void *);

		// Things related to (async) cogserver shell-evaluation
		const std::string *_pexpr;
		std::string _answer;

		void save_rc(SCM);
		SCM _rc;

		bool _eval_done;
		bool _poll_done;
		std::mutex _poll_mtx;
		std::condition_variable _wait_done;
		SCM _pipe;
		int _pipeno;
		void do_eval(const std::string &);
		std::string do_poll_result();
		std::string poll_port();
		static void * c_wrap_eval(void *);
		static void * c_wrap_poll(void *);

		// Support for interruption from a shell.
		SCM _eval_thread;
		static void * c_wrap_interrupt(void *);
		
		// Output port, for any printing done by scheme code.
		SCM _outport;
		SCM _saved_outport;
		bool _in_shell;
		bool _in_server;
		int _in_redirect;
		void capture_port();
		void redirect_output();
		void restore_output();
		void drain_output();

		// Straight-up evaluation
		SCM do_scm_eval(SCM, SCM (*)(void *));
		static void * c_wrap_eval_v(void *);
		static void * c_wrap_eval_as(void *);

		// Apply function to arguments, returning Handle or TV
		Handle _hargs;
		ValuePtr _retval;
		AtomSpace* _retas;
		SCM do_apply_scm(const std::string& func, const Handle& varargs);
		static void * c_wrap_apply_v(void *);

		// Exception and error handling stuff
		SCM _scm_error_string;
		std::string _error_msg;
		SCM _captured_stack;
		void set_error_string(SCM);
		void set_captured_stack(SCM);
		static SCM preunwind_handler_wrapper(void *, SCM, SCM);
		static SCM catch_handler_wrapper(void *, SCM, SCM);
		SCM preunwind_handler(SCM, SCM);
		SCM catch_handler(SCM, SCM);

		// Printing of basic types
		static std::string prt(SCM);

		static void * c_wrap_set_atomspace(void *);
		AtomSpace* _atomspace;
		int _gc_ctr;
		bool _in_eval;

	public:
		// Call before first use.
		static void init_scheme(void);

		// Set per-thread global
		static void set_scheme_as(AtomSpace*);

		SchemeEval(AtomSpace* = NULL);
		~SchemeEval();

		// Return per-thread, per-atomspace singleton
		static SchemeEval* get_evaluator(AtomSpace* = NULL);

		// The async-output interface.
		void begin_eval(void);
		void eval_expr(const std::string&);
		std::string poll_result(void);
		void interrupt(void);

		// The synchronous-output interfaces.
		std::string eval(const std::string& expr)
			{ begin_eval(); eval_expr(expr); return poll_result(); }
		std::string eval(const std::stringstream& ss)
			{ return eval(ss.str()); }

		// Evaluate expression, returning value.
		ValuePtr eval_v(const std::string&);
		ValuePtr eval_v(const std::stringstream& ss) { return eval_v(ss.str()); }

		// Evaluate expression, returning handle.
		Handle eval_h(const std::string& str) { return HandleCast(eval_v(str)); }
		Handle eval_h(const std::stringstream& ss) { return eval_h(ss.str()); }

		// Evaluate expression, returning TV.
		TruthValuePtr eval_tv(const std::string& str) { return TruthValueCast(eval_v(str)); }
		TruthValuePtr eval_tv(const std::stringstream& ss) { return eval_tv(ss.str()); }

		// Evaluate expression, returning AtomSpace.
		AtomSpace* eval_as(const std::string&);

		// Apply expression to args, returning Handle or TV
		virtual ValuePtr apply_v(const std::string& func, Handle varargs);
		Handle apply(const std::string& func, Handle varargs) {
			return HandleCast(apply_v(func, varargs)); }
		TruthValuePtr apply_tv(const std::string& func, Handle varargs) {
			return TruthValueCast(apply_v(func, varargs)); }

		// Nested invocations
		bool recursing(void) { return _in_eval; }
};

/** @}*/
}

extern "C" {
	// For shared-library loading
	opencog::SchemeEval* get_scheme_evaluator(opencog::AtomSpace*);
};

#endif/* HAVE_GUILE */

#endif /* OPENCOG_SCHEME_EVAL_H */
