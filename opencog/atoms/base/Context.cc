/*
 * opencog/atoms/base/Context.h
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

Context::Context(const Quotation& q, const OrderedHandleSet& s)
	: quotation(q), shadow(s) {}

void Context::update(const Handle& h)
{
	Type t = h->getType();

	// Update shadow
	if (quotation.is_unquoted() and classserver().isA(t, SCOPE_LINK)) {
		// Insert the new shadowing variables from the scope link
		const Variables& variables = ScopeLinkCast(h)->get_variables();
		shadow.insert(variables.varset.begin(), variables.varset.end());
	}

	// Update quotation
	quotation.update(t);
}

bool Context::is_free_variable(const Handle& h) const
{
	return (h->getType() == VARIABLE_NODE)
		and quotation.is_unquoted()
		and not is_in(h, shadow);
}

bool Context::operator==(const Context& context) const
{
	return (quotation == context.quotation)
		and ohs_content_eq(shadow, context.shadow);
}

bool Context::operator<(const Context& context) const
{
	return quotation < context.quotation
		or (quotation == context.quotation and shadow < context.shadow);
}

bool ohs_content_eq(const OrderedHandleSet& lhs, const OrderedHandleSet& rhs)
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

std::string oc_to_string(const Context& c)
{
	std::stringstream ss;
	if (c == Context())
		ss << "none" << std::endl;
	else
		ss << "quotation: " << oc_to_string(c.quotation) << std::endl
		   << "shadow:" << std::endl << oc_to_string(c.shadow);
	return ss.str();
}

} // namespace opencog
