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

#include <opencog/util/Logger.h>
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
	if (quotation.is_unquoted() and nameserver().isA(t, SCOPE_LINK)) {
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
	Type t = h->get_type();
	return (t == VARIABLE_NODE or t == GLOB_NODE)
		and quotation.is_unquoted()
		and not content_contains(HandleSeq(shadow.begin(), shadow.end()), h);
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

std::string oc_to_string(const Context::VariablesStack& scope_variables,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << scope_variables.size();
	int i = 0;
	for (const Variables& variables : scope_variables) {
		ss << std::endl << indent << "variables[" << i++ << "]:" << std::endl
		   << variables.to_string(indent + OC_TO_STRING_INDENT);
	}
	return ss.str();
}

std::string oc_to_string(const Context& c, const std::string& indent)
{
	std::stringstream ss;
	if (c == Context()) {
		ss << indent << "none";
	} else {
		ss << indent << "quotation: " << oc_to_string(c.quotation) << std::endl
		   << indent << "shadow:" << std::endl
		   << oc_to_string(c.shadow, indent + OC_TO_STRING_INDENT) << std::endl
		   << indent << "scope_variables:" << std::endl;
		if (c.store_scope_variables)
			ss << oc_to_string(c.scope_variables, indent + OC_TO_STRING_INDENT);
		else
			ss << indent + OC_TO_STRING_INDENT << "ignored";
	}
	return ss.str();
}

} // namespace opencog
