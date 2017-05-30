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

#include <map>
#include <set>
#include <stack>
#include <unordered_map>
#include <vector>

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/pattern/Pattern.h>
#include <opencog/query/PatternMatchCallback.h>

namespace opencog {

class PatternMatchEngine
{
	// -------------------------------------------
	// Callback to whom the results are reported.
	PatternMatchCallback &_pmc;
	ClassServer& _classserver;

	// Private, locally scoped typedefs, not used outside of this class.

private:
	// -------------------------------------------
	// The current set of clauses (redex context) being grounded.
	// A single redex consists of a collection of clauses, all of
	// which must be grounded.
	bool explore_redex(const Handle&, const Handle&, const Handle&);

	// These have to be pointers, not references; they get pushed
	// onto a stack when a new redex context is started. This is
	// how redex recursion will (eventually) be implemented.
	const Variables* _varlist;
	const Pattern* _pat;

	bool is_optional(const Handle& h) {
		return (_pat->optionals.count(h) != 0); }

	bool is_evaluatable(const Handle& h) {
		return (_pat->evaluatable_holders.count(h) != 0); }

	bool is_executable(const Handle& h) {
		return (_pat->executable_terms.count(h) != 0); }

	bool is_black(const Handle& h) {
		return (_pat->black.count(h) != 0); }

	// -------------------------------------------
	// Recursive redex support. These are stacks of the clauses
	// above, that are being searched.
	std::stack<const Variables*>  _stack_variables;
	std::stack<const Pattern*>    _stack_pattern;

	void push_redex(void);
	void pop_redex(void);

	// --------------------------------------------
	// Current clause traversal state. These hold the state needed
	// to traverse a single clause, and find groundings for it.
	// Note, though, that these are cumulative: so e.g. the
	// var_grounding map accumulates variable groundings for this
	// clause, and all previous clauses so far.

	// Map of current groundings of variables to their grounds
	// Also contains grounds of subclauses (not sure why, this seems
	// to be needed)
	HandleMap var_grounding;
	// Map of clauses to their current groundings
	HandleMap clause_grounding;

	void clear_current_state(void);  // clear the stuff above

	// -------------------------------------------
	// ChoiceLink state management
	typedef std::pair<PatternTermPtr, Handle> GndChoice;
	typedef std::map<GndChoice, size_t> ChoiceState;

	ChoiceState _choice_state;
	bool _need_choice_push;

	size_t curr_choice(const PatternTermPtr&, const Handle&, bool&);
	bool have_choice(const PatternTermPtr&, const Handle&);

	// Iteration control for choice links. Branchpoint advances
	// whenever take_step is set to true.
	bool choose_next;

	// -------------------------------------------
	// Unordered Link suppoprt
	typedef std::pair<PatternTermPtr, Handle> Unorder; // Choice
	typedef PatternTermSeq Permutation;
	typedef std::map<Unorder, Permutation> PermState; // ChoiceState

	PermState _perm_state;
	Permutation curr_perm(const PatternTermPtr&, const Handle&, bool&);
	bool have_perm(const PatternTermPtr&, const Handle&);

	// Iteration control for unordered links. Branchpoint advances
	// whenever take_step is set to true.
	bool take_step;
	bool have_more;
	std::map<Unorder, int> perm_count;
	std::stack<std::map<Unorder, int>> perm_count_stack;

	// --------------------------------------------
	// Methods and state that select the next clause to be grounded.

	bool do_next_clause(void);
	bool clause_accepted;
	void get_next_untried_clause(void);
	bool get_next_thinnest_clause(bool, bool, bool);
	unsigned int thickness(const Handle&, const OrderedHandleSet&);
	Handle next_clause;
	Handle next_joint;
	// Set of clauses for which a grounding is currently being attempted.
	typedef OrderedHandleSet IssuedSet;
	IssuedSet issued;     // stacked on issued_stack

	// -------------------------------------------
	// Stack used to store current traversal state for a single
	// clause. These are pushed when a clause is fully grounded,
	// and a new clause is about to be started. These are popped
	// in order to get back to the original clause, and resume
	// traversal of that clause, where it was last left off.
	void solution_push(void);
	void solution_pop(void);
	void solution_drop(void);

	// Stacks containing partial groundings.
	typedef HandleMap SolnMap;
	std::stack<SolnMap> var_solutn_stack;
	std::stack<SolnMap> term_solutn_stack;

	std::stack<IssuedSet> issued_stack;
	std::stack<ChoiceState> choice_stack;

	std::stack<PermState> perm_stack;
	void perm_push(void);
	void perm_pop(void);

	// push, pop and clear these states.
	void clause_stacks_push(void);
	void clause_stacks_pop(void);
	void clause_stacks_clear(void);
	unsigned int _clause_stack_depth;

	// -------------------------------------------
	// Recursive tree comparison algorithm.
	unsigned int depth; // Recursion depth for tree_compare.

	typedef enum {
		CALL_QUOTE,
		CALL_ORDER,
		CALL_GLOB,
		CALL_UNORDER,
		CALL_CHOICE,
		CALL_COMP,
		CALL_SOLN
	} Caller;   // temporary scaffolding !???

	bool tree_compare(const PatternTermPtr&, const Handle&, Caller);

	bool variable_compare(const Handle&, const Handle&);
	bool self_compare(const PatternTermPtr&);
	bool node_compare(const Handle&, const Handle&);
	bool choice_compare(const PatternTermPtr&, const Handle&);
	bool ordered_compare(const PatternTermPtr&, const Handle&);
	bool unorder_compare(const PatternTermPtr&, const Handle&);
	bool clause_compare(const PatternTermPtr&, const Handle&);

	// -------------------------------------------
	// Upwards-walking and grounding of a single clause.
	// See PatternMatchEngine.cc for descriptions
	bool explore_clause(const Handle&, const Handle&, const Handle&);
	bool explore_term_branches(const Handle&, const Handle&,
	                           const Handle&);
	bool explore_up_branches(const PatternTermPtr&, const Handle&,
	                         const Handle&);
	bool explore_link_branches(const PatternTermPtr&, const Handle&,
	                           const Handle&);
	bool explore_choice_branches(const PatternTermPtr&, const Handle&,
	                             const Handle&);
	bool explore_single_branch(const PatternTermPtr&, const Handle&,
	                           const Handle&);
	bool do_term_up(const PatternTermPtr&, const Handle&,
	                const Handle&);
	bool clause_accept(const Handle&, const Handle&);

public:
	PatternMatchEngine(PatternMatchCallback&);
	void set_pattern(const Variables&, const Pattern&);

	// Examine the locally connected neighborhood for possible
	// matches.
	bool explore_neighborhood(const Handle&, const Handle&, const Handle&);

	// Evaluate constant evaluatable and ground it via the
	// PatternMatchCallback. It is assumed that all clauses are
	// connected by an AndLink.
	bool explore_constant_evaluatables(const HandleSeq& clauses);

	// Handy-dandy utilities
	static void log_solution(const HandleMap &vars,
	                         const HandleMap &clauses);

	static void log_term(const OrderedHandleSet &vars,
	                     const HandleSeq &clauses);
};

} // namespace opencog

#endif // _OPENCOG_PATTERN_MATCH_ENGINE_H
