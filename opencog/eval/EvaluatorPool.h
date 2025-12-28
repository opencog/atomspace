/*
 * EvaluatorPool.h
 *
 * Pool for per-thread evaluators.
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

#ifndef _OPENCOG_EVALUATOR_POOL_H
#define _OPENCOG_EVALUATOR_POOL_H

#include "GenericEval.h"
#include <opencog/atomspace/AtomSpace.h>
#include <map>
#include <mutex>
#include <stack>

namespace opencog {
/** \addtogroup grp_server
 *  @{
 */

/**
 * Evaluator pool. Using a pool, instead of new/delete, is required in
 * order to work around nasty garbage-collection issues in scheme, and
 * also potentially in newer byte-code compiled python.
 *
 * Every thread gets it's own evaluator; when threads exit, one imagines
 * the evaluator can be freed. Not so easy -- the TLS thread destructor
 * has already run, and already passed through GC, before the Evaluator
 * dtor gets to run. This ends with an ugly crash in the dtor. Solution?
 * Don't dtor. Stick the Evaluator back into the pool, where it can be
 * recycled elsewhere. Minnor benefit? Avoids to overhead of running the
 * Evaluator ctor.
 *
 * Template? Well, each type needs to have its own pool; we can't mix
 * Scheme and Python evaluators in the same pool. Tried that already :-)
 * Wasn't happy about it :-)
 */
template <typename T>
class EvaluatorPool
{
private:
	static std::stack<T*> _pool;
	static std::mutex _pool_mtx;

	static T* get_from_pool();
	static void return_to_pool(T* ev);

public:
	static T* get_evaluator(const AtomSpacePtr& asp);
	static T* get_evaluator(AtomSpace* as);
};

/** @}*/

// Template implementation
template <typename T>
std::stack<T*> EvaluatorPool<T>::_pool;

template <typename T>
std::mutex EvaluatorPool<T>::_pool_mtx;

template <typename T>
T* EvaluatorPool<T>::get_from_pool()
{
	std::lock_guard<std::mutex> lock(_pool_mtx);

	if (!_pool.empty()) {
		T* ev = _pool.top();
		_pool.pop();
		return ev;
	}

	// Pool is empty; make a new one.
	return new T();
}

template <typename T>
void EvaluatorPool<T>::return_to_pool(T* ev)
{
	ev->set_atomspace(nullptr);
	ev->clear_pending();

	std::lock_guard<std::mutex> lock(_pool_mtx);
	_pool.push(ev);
}

// Return evaluator for this AtomSpace/thread combo.
template <typename T>
T* EvaluatorPool<T>::get_evaluator(const AtomSpacePtr& asp)
{
	static thread_local std::map<AtomSpacePtr, T*> issued;

	// The eval_dtor runs when this thread is destroyed.
	class eval_dtor {
		public:
		~eval_dtor() {
			for (auto ev : issued)
			{
				T* evaluator = ev.second;
				EvaluatorPool<T>::return_to_pool(evaluator);
			}
		}
	};
	static thread_local eval_dtor killer;

	auto ev = issued.find(asp);
	if (ev != issued.end())
		return ev->second;

	// Try to get from pool, or create new if pool is empty
	T* evaluator = EvaluatorPool<T>::get_from_pool();
	evaluator->set_atomspace(asp);
	issued[asp] = evaluator;

	return evaluator;
}

template <typename T>
T* EvaluatorPool<T>::get_evaluator(AtomSpace* as)
{
	OC_ASSERT(nullptr != as,
		"Cannot create evaluator without an AtomSpace!");

	const AtomSpacePtr& asp = AtomSpaceCast(as);
	return get_evaluator(asp);
}

}

#endif // _OPENCOG_EVALUATOR_POOL_H
