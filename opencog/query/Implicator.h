/*
 * Implicator.h
 *
 * Copyright (C) 2009, 2014 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#ifndef _OPENCOG_IMPLICATOR_H
#define _OPENCOG_IMPLICATOR_H

#include <vector>

#include <opencog/atomspace/AtomSpace.h>

#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/query/PatternMatchCallback.h>


namespace opencog {

/**
 * class Implicator -- pattern matching callback for grounding implicands.
 *
 * This class is meant to be used with the pattern matcher. When the
 * pattern matcher calls the callback, it will do so with a particular
 * grounding of the search pattern. If this class is holding an ungrounded
 * implicand, it will create a grounded version of the implicand. If
 * the implicand is already grounded, then it's a no-op -- this class
 * alone will *NOT* change its truth value.  Use a derived class for
 * this.
 *
 * The 'var_soln' argument in the callback contains the map from variables
 * to ground terms. 'class Instantiator' is used to perform the actual
 * grounding.  A set of grounded expressions is created in 'result_set'.
 * Note that the callback may be called many times reporting the same
 * results. In that case the 'result_set' will contain unique solutions.
 */
class Implicator :
	public virtual PatternMatchCallback
{
	protected:
		UnorderedHandleSet _result_set;
		HandleSeq _result_list;

	public:
		Implicator(AtomSpace* as) : inst(as), max_results(SIZE_MAX) {}
		Instantiator inst;
		Handle implicand;
		size_t max_results;

#ifdef CACHED_IMPLICATOR
		virtual void ready(AtomSpace* asp)
		{ inst.ready(asp); max_results = SIZE_MAX; }

		virtual void clear()
		{ inst.clear(); implicand = Handle::UNDEFINED; }
#endif

		virtual bool grounding(const HandleMap &var_soln,
		                       const HandleMap &term_soln);

		virtual void insert_result(const Handle&);
		virtual const UnorderedHandleSet& get_result_set() const
		{ return _result_set; }
		virtual const HandleSeq& get_result_list() const
		{ return _result_list; }
};

}; // namespace opencog

#endif // _OPENCOG_IMPLICATOR_H
