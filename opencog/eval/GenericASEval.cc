/*
 * opencog/eval/GenericASEval.cc
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, Inc.
 * All Rights Reserved
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

#include <map>
#include <mutex>

#include <opencog/util/concurrent_stack.h>
#include <opencog/atomspace/AtomSpace.h>
#include "GenericASEval.h"

using namespace opencog;

GenericASEval::GenericASEval(AtomSpace* as)
{
	// If it is coming from the pool, the as will be null.
	if (as)
		_atomspace = AtomSpaceCast(as);
	else
		_atomspace = nullptr;
}

GenericASEval::GenericASEval(AtomSpacePtr& asp)
{
	_atomspace = asp;
}

void GenericASEval::set_atomspace(const AtomSpacePtr& asp)
{
	_atomspace = asp;
}

AtomSpacePtr GenericASEval::get_atomspace(void)
{
	return _atomspace;
}

/* ============================================================== */

// A pool of evaluators, sitting hot and ready to go.
// This is used to implement get_evaluator(), below.  The only
// reason this is done with a pool, instead of simply new() and
// delete() is because calling delete() from TLS conflicts with
// the garbage collector in some language bindings, when the thread
// is destroyed. See the note below.
static concurrent_stack<GenericASEval*> pool;
static std::mutex pool_mtx;

GenericASEval* GenericASEval::get_from_pool(EvalFactory factory)
{
	std::lock_guard<std::mutex> lock(pool_mtx);
	GenericASEval* ev = NULL;
	if (pool.try_pop(ev)) return ev;
	return factory();
}

void GenericASEval::return_to_pool(GenericASEval* ev)
{
	ev->clear_pending();
	std::lock_guard<std::mutex> lock(pool_mtx);

	// try..catch is needed during library exit; the stack may
	// already be gone. So just ignore the resulting exception.
	// This should only happen during finalization.
	try {
		pool.push(ev);
	}
	catch (const concurrent_stack<GenericASEval*>::Canceled&) {}
}

/// Return evaluator, for this thread and atomspace combination.
/// If called with NULL, it will use the current atomspace for
/// this thread.
///
/// Use thread-local storage (TLS) in order to avoid repeatedly
/// creating and destroying the evaluator.
///
GenericASEval* GenericASEval::get_evaluator(const AtomSpacePtr& asp, EvalFactory factory)
{
	static thread_local std::map<AtomSpacePtr,GenericASEval*> issued;

	// The eval_dtor runs when this thread is destroyed.
	class eval_dtor {
		public:
		~eval_dtor() {
			for (auto ev : issued)
			{
				GenericASEval* evaluator = ev.second;

				// It would have been easier to just call delete evaluator
				// instead of return_to_pool.  Unfortunately, the delete
				// won't work, because the TLS thread destructor has already
				// run the garbage collector at this point, for this thread, and so
				// calling delete will lead to a crash in the destructor.
				// It would be nice if we got called before the GC did, but
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

	GenericASEval* evaluator = get_from_pool(factory);
	evaluator->_atomspace = asp;
	issued[asp] = evaluator;

	return evaluator;
}

GenericASEval* GenericASEval::get_evaluator(AtomSpace* as, EvalFactory factory)
{
	// A null AtomSpace is passed from the cython initialization
	// code. That code scrambles to create an AtomSpace, after
	// the evaluator is initialized.
	static AtomSpacePtr nullasp;
	if (nullptr == as) return get_evaluator(nullasp, factory);

	const AtomSpacePtr& asp = AtomSpaceCast(as);
	return get_evaluator(asp, factory);
}
