/*
 * GenericASEval.h
 *
 * Template for a generic AtomSpace-aware evaluator
 * Copyright (c) 2025 BrainyBlaze Dynamics, Inc.
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

#ifndef _OPENCOG_GENERIC_AS_EVAL_H
#define _OPENCOG_GENERIC_AS_EVAL_H

#include "GenericEval.h"
#include <opencog/atomspace/AtomSpace.h>

namespace opencog {
/** \addtogroup grp_server
 *  @{
 */

class GenericASEval : public GenericEval
{
	public:
		// Factory function type for creating new evaluators
		typedef GenericASEval* (*EvalFactory)();

	protected:
		AtomSpacePtr _atomspace;

		// Pool management
		static GenericASEval* get_from_pool(EvalFactory factory);
		static void return_to_pool(GenericASEval* ev);

	public:
		GenericASEval(AtomSpace*);
		GenericASEval(AtomSpacePtr&);
		virtual ~GenericASEval() {}

		virtual void set_atomspace(const AtomSpacePtr&);
		virtual AtomSpacePtr get_atomspace(void);

		// Return evaluator for this thread and atomspace combination.
		// Uses thread-local storage and a pool to avoid repeatedly
		// creating and destroying evaluators.
		static GenericASEval* get_evaluator(const AtomSpacePtr&, EvalFactory factory);
		static GenericASEval* get_evaluator(AtomSpace*, EvalFactory factory);
};

/** @}*/
}

#endif // _OPENCOG_GENERIC_AS_EVAL_H
