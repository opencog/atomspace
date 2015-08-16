/*
 * PatternMatchEngine.h
 *
 * Author: Linas Vepstas February 2008
 *
 * Copyright (C) 2008,2009 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_PATTERN_MATCH_ENGINE_H
#define _OPENCOG_PATTERN_MATCH_ENGINE_H

#include <opencog/query/Pattern.h>
#include <opencog/query/PatternMatchCallback.h>
#include <opencog/atomspace/ClassServer.h>

namespace opencog {

class PatternMatchEngine
{
	private:
		// -------------------------------------------
		// Callback to whom the results are reported.
		PatternMatchCallback &_pmc;
		ClassServer& _classserver;

		// These have to be pointers, not references
		// TODO: is it really needed to have pointers ???
		const Variables* _varlist;
		const Pattern* _pat;

		// -------------------------------------------
		// The current set of clauses (redex context) being grounded.
		// A single redex consists of a collection of clauses, all of
		// which must be grounded.
		bool explore_redex(const Handle&, const Handle&, const Handle&);

		// -------------------------------------------
		// Answers of recursive calls of backtracking algorithm
		typedef enum {
			NOT_MATCHED, // all searched until exhaustion and not found
			MATCHED,     // found a match and should search for more
			MATCHED_LAST,// found a match and do not want to search for more
			             // e.g. when the pattern matcher callback accepts
			             // only the first match
		} MatchStatus;

		// -------------------------------------------
		// Recursive calls of backtracking algorithm
		MatchStatus all_clauses_match(const PatternTermPtr&, const Handle&,
		                              const Handle&);
		MatchStatus single_clause_match(const PatternTermPtr&, const Handle&,
		                                const Handle&);

	public:
		PatternMatchEngine(PatternMatchCallback&,
		                   const Variables&,
		                   const Pattern&);

		// Examine the locally connected neighborhood for possible matches.
		bool explore_neighborhood(const Handle&, const Handle&, const Handle&);

};

} // namespace opencog

#endif // _OPENCOG_PATTERN_MATCH_ENGINE_H
