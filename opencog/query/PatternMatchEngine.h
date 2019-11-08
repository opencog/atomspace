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

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/pattern/Pattern.h>
#include <opencog/query/PatternMatchCallback.h>

namespace opencog {

class PatternMatchEngine
{
	// -------------------------------------------
	// Callback to whom the results are reported.
	PatternMatchCallback &_pmc;
	NameServer& _nameserver;

	// Private, locally scoped typedefs, not used outside of this class.

private:
	// -------------------------------------------
	// The pattern holds a collection of clauses that are to be
	// grounded. The variables are what are being directly grounded.

	// These are pointers; maybe they could be (should be?) references.
	const Variables* _varlist;
	const Pattern* _pat;

	bool is_optional(const Handle& h) {
		// return (_pat->optionals.count(h) != 0); }
		const HandleSeq& o(_pat->optionals);
		return o.end() != std::find(o.begin(), o.end(), h); }

	bool is_always(const Handle& h) {
		const HandleSeq& o(_pat->always);
		return o.end() != std::find(o.begin(), o.end(), h); }

	bool is_evaluatable(const Handle& h) {
		return (_pat->evaluatable_holders.count(h) != 0); }

	bool is_black(const Handle& h) {
		return (_pat->black.count(h) != 0); }

	bool term_is_a_clause(const PatternTermPtr&, const Handle&);

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

	// Insert association between pattern ptm and its grounding hg into
	// var_grounding.
	//
	// Takes quotation into account, that is only insert unquoted
	// pattern, and if it is quoted, attempt to restore the unquoted
	// pattern (as the quote is hidden inside the PatternTerm). This is
	// especially useful for recording subclause patterns, which turn
	// out to be useful during instantiation.
	void record_grounding(const PatternTermPtr& ptm, const Handle& hg);

	void clear_current_state(void);  // clear the stuff above

	// -------------------------------------------
	// ChoiceLink state management
	// Very similar to permutation state management.
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
	// Very similar to ChoiceLink state management.
	typedef std::pair<PatternTermPtr, Handle> Unorder; // alt: GndChoice
	typedef PatternTermSeq Permutation;
	typedef std::map<Unorder, Permutation> PermState; // alt: ChoiceState

	PermState _perm_state;
	Permutation curr_perm(const PatternTermPtr&, const Handle&, bool&);
	bool have_perm(const PatternTermPtr&, const Handle&);

	// Iteration control for unordered links. Branchpoint advances
	// whenever take_step is set to true.
	bool _take_step;
	bool _have_more;
	std::map<Unorder, int> _perm_count;
	std::stack<std::map<Unorder, int>> _perm_count_stack;

	// --------------------------------------------
	// Glob state management

	// Record the glob-pattern and the candidate we are comparing
	typedef std::pair<PatternTermSeq, HandleSeq> GlobPair;

	// Record where the globs are (branchpoints)
	typedef std::pair<PatternTermPtr, std::pair<size_t, size_t>> GlobPos;
	typedef std::stack<GlobPos> GlobPosStack;

	// Record how many atoms have been grounded to the globs
	typedef std::map<PatternTermPtr, size_t> GlobGrd;
	typedef std::pair<GlobGrd, GlobPosStack> GlobState;

	std::map<GlobPair, GlobState> _glob_state;

	// --------------------------------------------
	// Methods and state that select the next clause to be grounded.

	bool do_next_clause(void);
	bool clause_accepted;
	void get_next_untried_clause(void);
	Handle get_glob_embedding(const Handle&);
	bool get_next_thinnest_clause(bool, bool, bool);
	unsigned int thickness(const Handle&, const HandleSet&);
	Handle next_clause;
	Handle next_joint;
	// Set of clauses for which a grounding is currently being attempted.
	typedef HandleSet IssuedSet;
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
	std::stack<SolnMap> _clause_solutn_stack;

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
	// Methods that run when all clauses have been grounded.

	typedef HandleMap GrndMap;
	std::vector<GrndMap> _var_ground_cache;
	std::vector<GrndMap> _term_ground_cache;
	bool _forall_state = true;
	bool _did_check_forall;

	// Report a fully grounded pattern to the callback.
	bool report_grounding(const HandleMap &var_soln,
	                      const HandleMap &term_soln);
	bool report_forall(void);

	// -------------------------------------------
	// Recursive tree comparison algorithm.
	unsigned int depth; // Recursion depth for tree_compare.

	typedef enum {
		CALL_ORDER,
		CALL_GLOB,
		CALL_UNORDER,
		CALL_CHOICE,
		CALL_SOLN
	} Caller;   // temporary scaffolding !???

	bool tree_compare(const PatternTermPtr&, const Handle&, Caller);

	bool variable_compare(const Handle&, const Handle&);
	bool self_compare(const PatternTermPtr&);
	bool node_compare(const Handle&, const Handle&);
	bool choice_compare(const PatternTermPtr&, const Handle&);
	bool ordered_compare(const PatternTermPtr&, const Handle&);
	bool unorder_compare(const PatternTermPtr&, const Handle&);
	bool glob_compare(const PatternTermSeq&, const HandleSeq&);

	// -------------------------------------------
	// Upwards-walking and grounding of a single clause.
	// See PatternMatchEngine.cc for descriptions
	bool explore_clause(const Handle&, const Handle&, const Handle&);
	bool explore_redex(const Handle&, const Handle&, const Handle&);
	bool explore_term_branches(const Handle&, const Handle&,
	                           const Handle&);
	bool explore_up_branches(const PatternTermPtr&, const Handle&,
	                         const Handle&);
	bool explore_upvar_branches(const PatternTermPtr&, const Handle&,
	                         const Handle&);
	bool explore_upglob_branches(const PatternTermPtr&, const Handle&,
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
	static void print_solution(const HandleMap &vars,
	                           const HandleMap &clauses);
	static void log_solution(const HandleMap &vars,
	                         const HandleMap &clauses);

	static void log_term(const HandleSet &vars,
	                     const HandleSeq &clauses);
};

} // namespace opencog

#endif // _OPENCOG_PATTERN_MATCH_ENGINE_H
