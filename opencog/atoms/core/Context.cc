/*
 * opencog/atoms/core/Context.cc
 *
 * Copyright (C) 2017 OpenCog Foundation
 * All Rights Reserved
 *
 * Written by Nil Geisweiller
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

#include "Context.h"

#include <opencog/util/algorithm.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/core/Variables.h>
#include <opencog/atoms/core/ScopeLink.h>

#include <sstream>

namespace opencog {

Context::Context(const Quotation& q,
                 const HandleSet& s,
                 bool i, const VariablesStack& v)
	: quotation(q), shadow(s), store_scope_variables(i), scope_variables(v) {}

Context::Context(bool s) : store_scope_variables(s) {}

void Context::update(const Handle& h)
{
	Type t = h->get_type();

	// Update shadow
	if (quotation.is_unquoted() and classserver().isA(t, SCOPE_LINK)) {
		const Variables& variables = ScopeLinkCast(h)->get_variables();

		// Insert the new shadowing variables from the scope link
		shadow.insert(variables.varset.begin(), variables.varset.end());

		// Push the variables to scope_variables
		if (store_scope_variables)
			scope_variables.push_front(variables);
	}

	// Update quotation
	quotation.update(t);
}

bool Context::is_quoted() const
{
	return quotation.is_quoted();
}

bool Context::is_unquoted() const
{
	return quotation.is_unquoted();
}

bool Context::consumable(Type t) const
{
	return quotation.consumable(t);
}

bool Context::is_free_variable(const Handle& h) const
{
	return (h->get_type() == VARIABLE_NODE)
		and quotation.is_unquoted()
		and not is_in(h, shadow);
}

bool Context::operator==(const Context& other) const
{
	return (quotation == other.quotation)
		and ohs_content_eq(shadow, other.shadow)
		and // only look at scope variables if both care about it
		(not store_scope_variables or not other.store_scope_variables or
		 scope_variables == other.scope_variables);
}

bool Context::operator<(const Context& other) const
{
	return (quotation < other.quotation)
		or ((quotation == other.quotation and shadow < other.shadow)
		    // only look at scope variables if both care about it
		    or not store_scope_variables or not other.store_scope_variables
		    or (shadow == other.shadow and scope_variables < other.scope_variables));
}

bool ohs_content_eq(const HandleSet& lhs, const HandleSet& rhs)
{
	if (lhs.size() != rhs.size())
		return false;

	auto lit = lhs.begin();
	auto rit = rhs.begin();
	while (lit != lhs.end()) {
		if (not content_eq(*lit, *rit))
			return false;
		++lit; ++rit;
	}
	return true;
}

std::string oc_to_string(const Context::VariablesStack& scope_variables)
{
	std::stringstream ss;
	ss << "size = " << scope_variables.size() << std::endl;
	int i = 0;
	for (const Variables& variables : scope_variables) {
		ss << "variables[" << i++ << "]:" << std::endl
		   << variables.to_string();
	}
	return ss.str();
}

std::string oc_to_string(const Context& c)
{
	std::stringstream ss;
	if (c == Context()) {
		ss << "none" << std::endl;
	} else {
		ss << "quotation: " << oc_to_string(c.quotation) << std::endl
		   << "shadow:" << std::endl << oc_to_string(c.shadow);
		ss << "scope_variables:" << std::endl;
		if (c.store_scope_variables)
			ss << oc_to_string(c.scope_variables);
		else
			ss << "ignored" << std::endl;
	}
	return ss.str();
}

} // namespace opencog
