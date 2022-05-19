/*
 * GenericEval.h
 *
 * Template for a generic shell-oriented evaluator
 * Copyright (c) 2008, 2013, 2014 Linas Vepstas <linas@linas.org>
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

#ifndef _OPENCOG_GENERIC_EVAL_H
#define _OPENCOG_GENERIC_EVAL_H

#include <string>

/**
 * The GenericEval class implements a very simple API for language
 * evaluators.  Currently, its primary purpose in life is to make the
 * abstraction of the CogServer's GenericShell class possible.
 */

namespace opencog {
/** \addtogroup grp_server
 *  @{
 */

class GenericEval
{
	protected:
		std::string _input_line;
		std::string _error_string;
		bool _pending_input;
		bool _caught_error;

	public:
		GenericEval(void) :
			_input_line(""),
			_pending_input(false),
			_caught_error(false) {}
		virtual ~GenericEval() {}

		/**
		 * Return true if the expression was incomplete, and more is expected
		 * (for example, more closing parens are expected)
		 */
		virtual bool input_pending()
		{
			return _pending_input;
		}

		/**
		 * Clear the error state, the input buffers, etc.
		 */
		virtual void clear_pending()
		{
			_input_line = "";
			_error_string = "";
			_pending_input = false;
			_caught_error = false;
		}

		/**
		 * Return true if an error occurred during the evaluation of the expression
		 */
		virtual bool eval_error(void)
		{
			return _caught_error;
		}

		/**
		 * Return the error string, if any.
		 */
		virtual std::string get_error_string(void)
		{
			return _error_string;
		}

		/**
		 * The begin_eval() method must be called in the same thread
		 * as poll_result(), and it must be called before eval_expr().
		 *
		 * The eval_expr() method may be called in the same thread,
		 * or in a different one (as long as it is called after the
		 * begin_eval()).
		 *
		 * The poll_result() method can be called at any time after
		 * begin_eval(). (It might be called before eval_expr(), e.g.
		 * if the polling thread is racing with the evaluation thread.
		 * Such racing is allowed, and assumed to happen.)
		 *
		 * The poll_result() method is allowed to block, until
		 * results are available.  It must return the empty string
		 * to indicated that there are no more results.  After
		 * poll_result() returns the empty string, the caller can
		 * safely assume that the evaluation completed.
		 *
		 * The interrupt() method allows the caller to interrupt
		 * the thread that is running the eval_expr() method. If
		 * there is no thread that is running eval_expr, then the
		 * interrupt is a no-op (does nothing).
		 */
		virtual void begin_eval(void) = 0;
		virtual void eval_expr(const std::string&) = 0;
		virtual std::string poll_result(void) = 0;

		/**
		 * Implement a user-interrupt (ctrl-C at the keyboard) that
		 * will interrupt (kill, throw exception) whatever the eval
		 * above is currently running.  Typically, the interrupt is
		 * issued from a different thread than what is running eval.
		 */
		virtual void interrupt(void) = 0;
};

/** @}*/
}

#endif // _OPENCOG_GENERIC_EVAL_H
