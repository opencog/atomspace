/*
 * opencog/query/ConstraintDomain.cc
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, LLC
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <sstream>
#include <limits>

#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Atom.h>

#include "ConstraintDomain.h"

using namespace opencog;

// Static member initialization
const HandleSet ConstraintDomain::_empty_set;

// -------------------------------------------------------
// Domain initialization
// -------------------------------------------------------

void ConstraintDomain::init_domain(const Handle& var, const HandleSet& possible_values)
{
	_domains[var] = possible_values;
}

void ConstraintDomain::clear()
{
	_domains.clear();
	_bound_vars.clear();
	_neighbors.clear();
	while (!_state_stack.empty()) _state_stack.pop();
}

// -------------------------------------------------------
// Domain queries
// -------------------------------------------------------

const HandleSet& ConstraintDomain::get_domain(const Handle& var) const
{
	auto it = _domains.find(var);
	if (it == _domains.end()) return _empty_set;
	return it->second;
}

bool ConstraintDomain::has_domain(const Handle& var) const
{
	return _domains.find(var) != _domains.end();
}

bool ConstraintDomain::is_bound(const Handle& var) const
{
	auto it = _domains.find(var);
	if (it == _domains.end()) return false;
	return it->second.size() == 1;
}

bool ConstraintDomain::is_empty(const Handle& var) const
{
	auto it = _domains.find(var);
	if (it == _domains.end()) return true;
	return it->second.empty();
}

Handle ConstraintDomain::get_binding(const Handle& var) const
{
	auto it = _domains.find(var);
	if (it == _domains.end() || it->second.size() != 1)
		return Handle::UNDEFINED;
	return *(it->second.begin());
}

size_t ConstraintDomain::domain_size(const Handle& var) const
{
	auto it = _domains.find(var);
	if (it == _domains.end()) return 0;
	return it->second.size();
}

// -------------------------------------------------------
// Constraint network structure
// -------------------------------------------------------

void ConstraintDomain::add_constraint(const HandleSeq& variables)
{
	// Each variable in the constraint is a neighbor of all others
	for (const Handle& v1 : variables)
	{
		for (const Handle& v2 : variables)
		{
			if (v1 != v2)
				_neighbors[v1].insert(v2);
		}
	}
}

HandleSet ConstraintDomain::get_neighbors(const Handle& var) const
{
	auto it = _neighbors.find(var);
	if (it == _neighbors.end()) return HandleSet();
	return it->second;
}

// -------------------------------------------------------
// Constraint propagation
// -------------------------------------------------------

bool ConstraintDomain::bind(const Handle& var, const Handle& value)
{
	auto it = _domains.find(var);
	if (it == _domains.end())
	{
		// Variable not tracked - just record single-value domain
		_domains[var] = {value};
		_bound_vars.insert(var);
		return true;
	}

	// Check that value is in domain
	if (it->second.find(value) == it->second.end())
	{
		// Value not in domain - conflict!
		return false;
	}

	// Set domain to single value
	it->second = {value};
	_bound_vars.insert(var);

	// Propagate: remove this value from all neighbors' domains
	auto neighbors_it = _neighbors.find(var);
	if (neighbors_it != _neighbors.end())
	{
		for (const Handle& neighbor : neighbors_it->second)
		{
			if (!eliminate(neighbor, value))
				return false;  // Neighbor's domain became empty
		}
	}

	return true;
}

bool ConstraintDomain::eliminate(const Handle& var, const Handle& value)
{
	auto it = _domains.find(var);
	if (it == _domains.end()) return true;  // Not tracking this var

	// If already bound to something else, we're fine
	if (_bound_vars.find(var) != _bound_vars.end())
		return true;

	// Remove value from domain
	it->second.erase(value);

	// Check for empty domain (conflict)
	if (it->second.empty())
		return false;

	return true;
}

Handle ConstraintDomain::find_unit() const
{
	for (const auto& kv : _domains)
	{
		if (kv.second.size() == 1 &&
		    _bound_vars.find(kv.first) == _bound_vars.end())
		{
			return kv.first;
		}
	}
	return Handle::UNDEFINED;
}

Handle ConstraintDomain::most_constrained() const
{
	Handle best = Handle::UNDEFINED;
	size_t min_size = std::numeric_limits<size_t>::max();

	for (const auto& kv : _domains)
	{
		// Skip bound variables and empty domains
		if (_bound_vars.find(kv.first) != _bound_vars.end())
			continue;
		if (kv.second.size() <= 1)
			continue;

		if (kv.second.size() < min_size)
		{
			min_size = kv.second.size();
			best = kv.first;
		}
	}

	return best;
}

// -------------------------------------------------------
// State save/restore for backtracking
// -------------------------------------------------------

void ConstraintDomain::push_state()
{
	State state;
	state.domains = _domains;
	state.bound_vars = _bound_vars;
	_state_stack.push(state);
}

void ConstraintDomain::pop_state()
{
	if (_state_stack.empty()) return;
	State& state = _state_stack.top();
	_domains = state.domains;
	_bound_vars = state.bound_vars;
	_state_stack.pop();
}

void ConstraintDomain::pop_discard()
{
	if (!_state_stack.empty())
		_state_stack.pop();
}

// -------------------------------------------------------
// Debugging
// -------------------------------------------------------

std::string ConstraintDomain::to_string() const
{
	std::ostringstream oss;
	oss << "ConstraintDomain {\n";

	for (const auto& kv : _domains)
	{
		oss << "  " << kv.first->to_short_string() << " : {";
		bool first = true;
		for (const Handle& h : kv.second)
		{
			if (!first) oss << ", ";
			first = false;
			oss << h->to_short_string();
		}
		oss << "}";
		if (_bound_vars.find(kv.first) != _bound_vars.end())
			oss << " [BOUND]";
		oss << "\n";
	}

	oss << "  Constraints:";
	for (const auto& kv : _neighbors)
	{
		oss << "\n    " << kv.first->to_short_string() << " <-> ";
		bool first = true;
		for (const Handle& h : kv.second)
		{
			if (!first) oss << ", ";
			first = false;
			oss << h->to_short_string();
		}
	}
	oss << "\n}\n";

	return oss.str();
}
