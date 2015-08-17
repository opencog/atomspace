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

#include <set>
#include <stack>

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

		// These have to be pointers, not references; they get pushed
		// onto a stack when a new redex context is started. This is
		// how redex recursion will (eventually) be implemented.
		// TODO: is it really needed to have pointers ???
		const Variables* _varlist;
		const Pattern* _pat;

		bool is_optional(const Handle& h) {
			return (_pat->optionals.count(h) != 0); }
		bool is_evaluatable(const Handle& h) {
			return (_pat->evaluatable_holders.count(h) != 0); }
		bool is_black(const Handle& h) {
			return (_pat->black.count(h) != 0); }

		// --------------------------------------------
		// Current clause traversal state. These hold the state needed
		// to traverse a single clause, and find groundings for it.
		// Note, though, that these are cumulative: so e.g. the
		// var_grounding map accumulates variable groundings for this
		// clause, and all previous clauses so far.

		// Map of current groundings of variables to their grounds
		std::map<Handle, Handle> var_grounding;
		// Map of clauses to their current groundings
		std::map<Handle, Handle> clause_grounding;

		// --------------------------------------------
		// Methods and state that select the next clause to be grounded.

		void get_next_untried_clause(void);
		bool get_next_thinnest_clause(bool, bool, bool);
		unsigned int thickness(const Handle&, const std::set<Handle>&);
		Handle next_clause;
		Handle next_joint;

		// Stack and set of clauses for which a grounding is currently
		// being attempted.
		void push_next_clause(void);
		void pop_clause(void);
		typedef std::stack<Handle> IssuedStack;
		typedef std::set<Handle> IssuedSet;
		IssuedStack clauses_stack;
		IssuedStack joints_stack;
		IssuedSet issued_clauses;     // stacked on clauses_stack

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
			             // e.g. should halt when the pattern matcher callback
			             // found the grounding acceptable
		} MatchStatus;

		// -------------------------------------------
		// Recursive calls of backtracking algorithm
		MatchStatus all_clauses_match(const Handle&, const Handle&,
		                              const Handle&);
		MatchStatus clause_match(const Handle&, const Handle&,
		                         const Handle&);

		bool clause_accept(const Handle&, const Handle&, const Handle&);

	public:
		PatternMatchEngine(PatternMatchCallback&,
		                   const Variables&,
		                   const Pattern&);

		// Examine the locally connected neighborhood for possible matches.
		bool explore_neighborhood(const Handle&, const Handle&, const Handle&);

		// Handy-dandy utilities
		static void print_solution(const std::map<Handle, Handle> &vars,
		                           const std::map<Handle, Handle> &clauses);

};

} // namespace opencog

#endif // _OPENCOG_PATTERN_MATCH_ENGINE_H
