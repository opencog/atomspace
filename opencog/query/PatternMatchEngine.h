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
	const Variables* _variables;
	const Pattern* _pat;

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
	GroundingMap var_grounding;
	// Map of clauses to their current groundings
	GroundingMap clause_grounding;

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
	// Similar to permutation state management.
	typedef std::map<PatternTermPtr, size_t> ChoiceState;

	ChoiceState _choice_state;
	bool _need_choice_push;

	size_t curr_choice(const PatternTermPtr&, const Handle&);
	bool have_choice(const PatternTermPtr&, const Handle&);

	// Iteration control for choice links. Branchpoint advances
	// whenever _choose_next is set to true.
	bool _choose_next;

	// -------------------------------------------
	// Unordered Link support
	// Similar to ChoiceLink state management.
	typedef PatternTermSeq Permutation;
	typedef std::map<PatternTermPtr, Permutation> PermState; // alt: ChoiceState
	typedef std::map<PatternTermPtr, int> PermCount;
	typedef std::map<PatternTermPtr, bool> PermOdo;
	typedef std::map<PatternTermPtr, PermOdo> PermOdoState;

	PermState _perm_state;
	Permutation curr_perm(const PatternTermPtr&, const Handle&);
	bool have_perm(const PatternTermPtr&, const Handle&);

	// Iteration control for unordered links. Branchpoint advances
	// whenever take_step is set to true.
	bool _perm_take_step;
	bool _perm_have_more;
	bool _perm_go_around;
	PatternTermPtr _perm_to_step;
	std::stack<PatternTermPtr> _perm_step_saver;
	PatternTermPtr _perm_breakout;

	PermOdo _perm_odo;
	PermOdo _perm_podo;
	PermOdoState _perm_odo_state;

	std::stack<bool> _perm_take_stack;
	std::stack<bool> _perm_more_stack;
	std::stack<PatternTermPtr> _perm_stepper_stack;
	std::stack<PatternTermPtr> _perm_breakout_stack;
	std::stack<PermOdoState> _perm_odo_stack;

	std::stack<PermState> _perm_stack;
	PermCount _perm_count;
	std::stack<PermCount> _perm_count_stack;

	void perm_push(void);
	void perm_pop(void);

	// --------------------------------------------
	// Glob state management

	// Record where the globs are (branchpoints)
	typedef std::pair<PatternTermPtr, std::pair<size_t, size_t>> GlobPos;
	typedef std::stack<GlobPos> GlobPosStack;

	// Record how many atoms have been grounded to the globs
	typedef std::map<PatternTermPtr, size_t> GlobGrd;
	typedef std::pair<GlobGrd, GlobPosStack> GlobState;

	// GlobState can be defined as either std::map (aka std::_Rb_tree)
	// or as std::unordered_map (aka std::_Hashtable). I looked for a
	// performance difference between these two, but could not find one,
	// at least with the `guile -l nano-en.scm` benchmark.
	// (As of Dec 2019, using gcc-8.3.0 and glibc-2.28)
	std::map<PatternTermSeq, GlobState> _glob_state;
	// std::unordered_map<PatternTermSeq, GlobState> _glob_state;

	// --------------------------------------------
	// Sparse matching state management
	// Similar to choice, unordered and glob state management.
	typedef std::vector<int> Selection;
	typedef std::map<PatternTermPtr, Selection> SparseState; // alt: ChoiceState
	typedef std::map<PatternTermPtr, Handle> SparseGlob;
	typedef std::map<PatternTermPtr, PatternTermSeq> SparseTerm;

	SparseState _sparse_state;
	SparseGlob _sparse_glob;
	SparseTerm _sparse_term;

	bool setup_select(const PatternTermPtr&, const Handle&);
	Selection curr_select(const PatternTermPtr&);
	Handle curr_sparse_glob(const PatternTermPtr&);
	SparseTerm curr_sparse_term(const PatternTermPtr&);
	bool have_select(const PatternTermPtr&);

	// --------------------------------------------
	// Methods and state that select the next clause to be grounded.
	bool do_next_clause(void);
	bool clause_accepted;

	// --------------------------------------------
	// State that manages the next PresentLink subterm to be grounded.
	// Similar to the next-clause, above, and someday should be unified
	// with it. XXX Needs to move to the Mixin class... XX FIXME.

	bool next_untried_present(const PatternTermPtr&,
	                          const PatternTermPtr&,
	                          PatternTermPtr&, PatternTermPtr&,
	                          Handle&);
	typedef std::set<PatternTermPtr> IssuedSet;
	IssuedSet issued_present;

	// -------------------------------------------
	// Methods that help avoid pointless searches
	bool is_clause_grounded(const PatternTermPtr&) const;
	HandleSeq clause_grounding_key(const Handle&,
	                               const HandleSeq&) const;

	// Positive and negative caches of clauses.
	std::unordered_map<HandleSeq, Handle> _gnd_cache;
	std::unordered_set<HandleSeq> _nack_cache;

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
	std::stack<GroundingMap> var_solutn_stack;
	std::stack<GroundingMap> _clause_solutn_stack;

	std::stack<ChoiceState> choice_stack;

	// push, pop and clear these states.
	void clause_stacks_push(void);
	void clause_stacks_pop(void);
	void clause_stacks_clear(void);
	unsigned int _clause_stack_depth;

	// -------------------------------------------
	// Methods that run when all clauses have been grounded.

	std::vector<GroundingMap> _var_ground_cache;
	std::vector<GroundingMap> _term_ground_cache;
	bool _forall_state = true;
	bool _did_check_forall;

	// Report a fully grounded pattern to the callback.
	bool report_grounding(const GroundingMap &var_soln,
	                      const GroundingMap &term_soln);
	bool report_forall(void);

	// -------------------------------------------
	// Recursive tree comparison algorithm.
	unsigned int depth; // Recursion depth for tree_compare.

	typedef enum {
		CALL_ORDER,
		CALL_GLOB,
		CALL_ELIM,
		CALL_UNORDER,
		CALL_PRESENT,
		CALL_CHOICE,
		CALL_SOLN
	} Caller;   // debug-print call-tracing.

	bool tree_compare(const PatternTermPtr&, const Handle&, Caller);

	bool variable_compare(const Handle&, const Handle&);
	bool self_compare(const PatternTermPtr&);
	bool node_compare(const Handle&, const Handle&);
	bool present_compare(const PatternTermPtr&, const Handle&);
	bool choice_compare(const PatternTermPtr&, const Handle&);
	bool ordered_compare(const PatternTermPtr&, const Handle&);
	bool unorder_compare(const PatternTermPtr&, const Handle&);
	bool sparse_compare(const PatternTermPtr&, const Handle&);
	bool glob_compare(const PatternTermSeq&, const HandleSeq&);

	bool elim_compare(const PatternTermPtr&, const Handle&,
	                  const PatternTermSeq&);
	bool record_elim(const PatternTermPtr&, const Handle&);
	// -------------------------------------------
	// Upwards-walking and grounding of a single clause.
	// See PatternMatchEngine.cc for descriptions
	bool explore_clause(const PatternTermPtr&, const Handle&,
	                    const PatternTermPtr&);
	bool explore_clause_direct(const PatternTermPtr&, const Handle&,
	                           const PatternTermPtr&);
	bool explore_clause_evaluatable(const PatternTermPtr&, const Handle&,
	                                const PatternTermPtr&);
	bool explore_clause_identical(const PatternTermPtr&, const Handle&,
	                              const PatternTermPtr&);
	bool explore_term_branches(const PatternTermPtr&, const Handle&,
	                           const PatternTermPtr&);
	bool explore_up_branches(const PatternTermPtr&, const Handle&,
	                         const PatternTermPtr&);
	bool explore_upvar_branches(const PatternTermPtr&, const Handle&,
	                            const PatternTermPtr&);
	bool explore_upglob_branches(const PatternTermPtr&, const Handle&,
	                             const PatternTermPtr&);
	bool explore_glob_branches(const PatternTermPtr&, const Handle&,
	                           const PatternTermPtr&);
	bool explore_sparse_branches(const PatternTermPtr&, const Handle&,
	                             const PatternTermPtr&);
	bool explore_type_branches(const PatternTermPtr&, const Handle&,
	                           const PatternTermPtr&);
	bool explore_odometer(const PatternTermPtr&, const Handle&,
	                      const PatternTermPtr&);
	bool explore_unordered_branches(const PatternTermPtr&, const Handle&,
	                                const PatternTermPtr&);
	bool explore_choice_branches(const PatternTermPtr&, const Handle&,
	                             const PatternTermPtr&);
	bool explore_present_branches(const PatternTermPtr&, const Handle&,
	                              const PatternTermPtr&);
	bool explore_single_branch(const PatternTermPtr&, const Handle&,
	                           const PatternTermPtr&);
	bool do_term_up(const PatternTermPtr&, const Handle&,
	                const PatternTermPtr&);
	bool clause_accept(const PatternTermPtr&, const Handle&);

public:
	PatternMatchEngine(PatternMatchCallback&);
	void set_pattern(const Variables&, const Pattern&);

	// Examine the locally connected neighborhood for possible
	// matches.
	bool explore_neighborhood(const PatternTermPtr&, const Handle&,
	                          const PatternTermPtr&);

	// Evaluate constant evaluatable and ground it via the
	// PatternMatchCallback. It is assumed that all clauses are
	// connected by an AndLink.
	bool explore_constant_evaluatables(const PatternTermSeq& clauses);

	// Handy-dandy utilities
	static void print_solution(const GroundingMap &vars,
	                           const GroundingMap &clauses);
	static void log_solution(const GroundingMap &vars,
	                         const GroundingMap &clauses);

	static void log_term(const HandleSet &vars,
	                     const HandleSeq &clauses);
};

} // namespace opencog

#endif // _OPENCOG_PATTERN_MATCH_ENGINE_H
