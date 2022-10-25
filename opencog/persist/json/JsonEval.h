/*
 * JsonEval.h
 *
 * A simple JSON shell-oriented evaluator
 * Copyright (c) 2008, 2013, 2014, 2020, 2021 Linas Vepstas <linas@linas.org>
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

#ifndef _OPENCOG_JSON_EVAL_H
#define _OPENCOG_JSON_EVAL_H

#include <mutex>
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/eval/GenericEval.h>

/**
 * The JsonEval class implements a very simple API for Javascript
 * evaluation.  It supports just enough commands to allow AtomSpaces
 * and portions thereof to be easily transported across the network.
 * It is used by the CogServer, and the atomspace-js network backend.
 */

namespace opencog {
/** \addtogroup grp_server
 *  @{
 */

class AtomSpace;
class JsonEval : public GenericEval
{
	private:
		AtomSpacePtr _atomspace;

		// poll_result() is called in a different thread
		// than eval_expr() and the result is that _answer
		// can get clobbered. So force the reader to wait.
		std::mutex _mtx;
		std::string _answer;

		JsonEval(const AtomSpacePtr&);
	public:
		virtual ~JsonEval();

		virtual void begin_eval(void);
		virtual void eval_expr(const std::string&);
		virtual std::string poll_result(void);

		virtual void interrupt(void);

		static JsonEval* get_evaluator(const AtomSpacePtr&);
};

/** @}*/
}

#endif // _OPENCOG_JSON_EVAL_H
