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
#include <map>
#include <mutex>
#include <stack>

namespace opencog {
/** \addtogroup grp_server
 *  @{
 */

/**
 * Template-based pool for evaluators.
 * Each evaluator type (SchemeEval, PythonEval, etc.) gets its own pool
 * through template instantiation, avoiding mixing of different evaluator types.
 */
template <typename T>
class GenericEvalPool
{
private:
	static std::stack<T*> _pool;
	static std::mutex _pool_mtx;

	static T* get_from_pool();
	static void return_to_pool(T* ev);

public:
	// Return evaluator for this thread and atomspace combination.
	// Uses thread-local storage and pool to avoid repeatedly
	// creating and destroying evaluators.
	static T* get_evaluator(const AtomSpacePtr& asp);
	static T* get_evaluator(AtomSpace* as);
};

class GenericASEval : public GenericEval
{
	protected:
		AtomSpacePtr _atomspace;

	public:
		GenericASEval(AtomSpace*);
		GenericASEval(AtomSpacePtr&);
		virtual ~GenericASEval() {}

		virtual void set_atomspace(const AtomSpacePtr&);
		virtual AtomSpacePtr get_atomspace(void);
};

/** @}*/

// Template implementation must be in header
template <typename T>
std::stack<T*> GenericEvalPool<T>::_pool;

template <typename T>
std::mutex GenericEvalPool<T>::_pool_mtx;

template <typename T>
T* GenericEvalPool<T>::get_from_pool()
{
	std::lock_guard<std::mutex> lock(_pool_mtx);

	if (!_pool.empty()) {
		T* ev = _pool.top();
		_pool.pop();
		return ev;
	}

	// Pool is empty, return nullptr to signal caller should create new instance
	return nullptr;
}

template <typename T>
void GenericEvalPool<T>::return_to_pool(T* ev)
{
	ev->clear_pending();

	std::lock_guard<std::mutex> lock(_pool_mtx);
	_pool.push(ev);
}

template <typename T>
T* GenericEvalPool<T>::get_evaluator(const AtomSpacePtr& asp)
{
	static thread_local std::map<AtomSpacePtr, T*> issued;

	// The eval_dtor runs when this thread is destroyed.
	class eval_dtor {
		public:
		~eval_dtor() {
			for (auto ev : issued)
			{
				T* evaluator = ev.second;

				// Return to pool instead of delete to avoid GC conflicts
				evaluator->set_atomspace(nullptr);
				GenericEvalPool<T>::return_to_pool(evaluator);
			}
		}
	};
	static thread_local eval_dtor killer;

	auto ev = issued.find(asp);
	if (ev != issued.end())
		return ev->second;

	// Try to get from pool, or create new if pool is empty
	T* evaluator = GenericEvalPool<T>::get_from_pool();
	if (!evaluator)
		evaluator = new T(nullptr);

	evaluator->set_atomspace(asp);
	issued[asp] = evaluator;

	return evaluator;
}

template <typename T>
T* GenericEvalPool<T>::get_evaluator(AtomSpace* as)
{
	OC_ASSERT(nullptr != as,
		"Cannot create evaluator without an AtomSpace!");

	const AtomSpacePtr& asp = AtomSpaceCast(as);
	return get_evaluator(asp);
}

}

#endif // _OPENCOG_GENERIC_AS_EVAL_H
