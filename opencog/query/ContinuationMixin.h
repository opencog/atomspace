/*
 * ContinuationMixin.h
 *
 * Copyright (C) 2021 Linas Vepstas <linasvepstas@gmail.com>
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
 *
 * Created by Linas Vepstas November 2021
 */

#ifndef _OPENCOG_CONTINUATION_MIXIN_H
#define _OPENCOG_CONTINUATION_MIXIN_H

#include <opencog/query/InitiateSearchMixin.h>
#include <opencog/query/SatisfyMixin.h>
#include <opencog/query/TermMatchMixin.h>

namespace opencog {

class AtomSpace;

/**
 * Callback mixin class, used to provide continuations for infinite
 * recursion. It provides a short-circuit path from deep within the
 * search to the conclusion of the search.
 */
class ContinuationMixin : 
	public TermMatchMixin,
	public InitiateSearchMixin,
	public SatisfyMixin
{
	public:
		ContinuationMixin(AtomSpace* as) :
			TermMatchMixin(as), InitiateSearchMixin(as),
			_continuation(nullptr)
		{}

		virtual void set_pattern(const Variables& vars,
		                         const Pattern& pat)
		{
			TermMatchMixin::set_pattern(vars, pat);
			InitiateSearchMixin::set_pattern(vars, pat);
		}

		/**
		 * Called to perform the actual search. Continuations
		 * return from here.
		 */
		virtual bool perform_search(PatternMatchCallback&);

		/**
		 * Continuations exit from here.
		 */
		virtual bool evaluate_sentence(const Handle&, const GroundingMap&);

		/**
		 * Continuations enter here.
		 */
		virtual bool satisfy(const PatternLinkPtr&);

	protected:
		Handle _continuation;
};

} // namespace opencog

#endif // _OPENCOG_CONTINUATION_MIXIN_H
