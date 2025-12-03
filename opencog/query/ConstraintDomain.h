/*
 * opencog/query/ConstraintDomain.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, LLC
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Constraint propagation support for pattern matching.
 * See README-constraint.md for design overview.
 */

#ifndef _OPENCOG_CONSTRAINT_DOMAIN_H
#define _OPENCOG_CONSTRAINT_DOMAIN_H

#include <map>
#include <set>
#include <stack>
#include <vector>

#include <opencog/atoms/base/Handle.h>

namespace opencog
{

/**
 * DomainMap - tracks possible groundings for each variable.
 *
 * Unlike GroundingMap which maps variable -> single grounding,
 * DomainMap maps variable -> set of possible groundings.
 */
typedef std::map<Handle, HandleSet> DomainMap;

/**
 * ConstraintDomain - manages constraint propagation during pattern matching.
 *
 * This class tracks the "domain" (set of possible values) for each
 * variable in a pattern. When a variable is bound, constraint
 * propagation removes that value from the domains of related variables.
 *
 * The constraint network is defined by which variables appear together
 * in UnorderedLinks (SetLinks). If {$X, $Y, $Z} must match {a, b, c},
 * then binding $X=a means $Y and $Z can only be {b, c}.
 */
class ConstraintDomain
{
public:
	ConstraintDomain() = default;

	// -------------------------------------------------------
	// Domain initialization
	// -------------------------------------------------------

	/**
	 * Initialize domain for a variable with a set of possible values.
	 */
	void init_domain(const Handle& var, const HandleSet& possible_values);

	/**
	 * Clear all domains (reset state).
	 */
	void clear();

	/**
	 * Save current state as initial state (call after setup).
	 */
	void save_initial();

	/**
	 * Reset to initial state (for starting new search).
	 */
	void reset();

	// -------------------------------------------------------
	// Domain queries
	// -------------------------------------------------------

	/**
	 * Get the current domain (possible values) for a variable.
	 * Returns empty set if variable is not tracked.
	 */
	const HandleSet& get_domain(const Handle& var) const;

	/**
	 * Check if variable has a domain registered.
	 */
	bool has_domain(const Handle& var) const;

	/**
	 * Check if any domains have been registered.
	 */
	bool empty() const { return _domains.empty(); }

	/**
	 * Check if variable is bound (domain has exactly one value).
	 */
	bool is_bound(const Handle& var) const;

	/**
	 * Check if variable has empty domain (conflict detected).
	 */
	bool is_empty(const Handle& var) const;

	/**
	 * Get the binding for a variable (only valid if is_bound()).
	 */
	Handle get_binding(const Handle& var) const;

	/**
	 * Get domain size for a variable.
	 */
	size_t domain_size(const Handle& var) const;

	// -------------------------------------------------------
	// Constraint network structure
	// -------------------------------------------------------

	/**
	 * Register that a set of variables are constrained together
	 * (e.g., they appear in the same UnorderedLink and must take
	 * distinct values from the same set).
	 */
	void add_constraint(const HandleSeq& variables);

	/**
	 * Get all variables that share a constraint with the given variable.
	 */
	HandleSet get_neighbors(const Handle& var) const;

	// -------------------------------------------------------
	// Constraint propagation
	// -------------------------------------------------------

	/**
	 * Bind a variable to a specific value.
	 * Removes this value from domains of all neighboring variables.
	 * Returns false if this causes any domain to become empty (conflict).
	 */
	bool bind(const Handle& var, const Handle& value);

	/**
	 * Remove a value from a variable's domain.
	 * If this leaves only one value, does NOT auto-propagate (call bind).
	 * Returns false if domain becomes empty.
	 */
	bool eliminate(const Handle& var, const Handle& value);

	/**
	 * Find a variable with domain size == 1 that hasn't been bound yet.
	 * Returns Handle::UNDEFINED if none found.
	 * This is for "unit propagation" - binding forced variables.
	 */
	Handle find_unit() const;

	/**
	 * Find the variable with smallest domain size > 1 (MRV heuristic).
	 * Returns Handle::UNDEFINED if all variables are bound or empty.
	 */
	Handle most_constrained() const;

	// -------------------------------------------------------
	// State save/restore for backtracking
	// -------------------------------------------------------

	/**
	 * Save current domain state for later restore.
	 */
	void push_state();

	/**
	 * Restore previously saved domain state.
	 */
	void pop_state();

	/**
	 * Discard saved state without restoring.
	 */
	void pop_discard();

	// -------------------------------------------------------
	// Debugging
	// -------------------------------------------------------

	/**
	 * Print current domain state for debugging.
	 */
	std::string to_string() const;

private:
	// Current domains: variable -> set of possible values
	DomainMap _domains;

	// Initial domains (saved after setup, for reset)
	DomainMap _initial_domains;

	// Variables known to be bound (domain size == 1 and propagated)
	HandleSet _bound_vars;

	// Constraint graph: variable -> neighboring variables
	std::map<Handle, HandleSet> _neighbors;

	// Stack of saved states for backtracking
	struct State {
		DomainMap domains;
		HandleSet bound_vars;
	};
	std::stack<State> _state_stack;

	// Empty set for returning const reference
	static const HandleSet _empty_set;
};

} // namespace opencog

#endif // _OPENCOG_CONSTRAINT_DOMAIN_H
