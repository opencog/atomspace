/*
 * opencog/atoms/scope/FilterVardecl.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  December 2015
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/base/Link.h>

#include <opencog/atoms/free/FindUtils.h>

#include "FilterVardecl.h"

namespace opencog {

Handle filter_vardecl(const Handle& vardecl, const Handle& body)
{
	return filter_vardecl(vardecl, HandleSeq{body});
}

// See also `Variables::trim()`, which does the same thing,
// conceptually. The difference is trim() cuts down the vardecl
// in-place, instead of building a new one, like below.
Handle filter_vardecl(const Handle& vardecl, const HandleSeq& hs)
{
	// Base cases

	if (not vardecl)
		// Return Handle::UNDEFINED to indicate that this variable
		// declaration is nonexistent.
		return Handle::UNDEFINED;

	Type t = vardecl->get_type();
	if ((VARIABLE_NODE == t) || (GLOB_NODE == t))
	{
		if (is_free_in_any_tree(hs, vardecl))
			return vardecl;
	}

	// Recursive cases

	else if (TYPED_VARIABLE_LINK == t)
	{
		Handle var = vardecl->getOutgoingAtom(0);
		Type t = var->get_type();
		if (((t == VARIABLE_NODE) || (t == GLOB_NODE)) and filter_vardecl(var, hs))
			return vardecl;
	}

	else if (VARIABLE_LIST == t or VARIABLE_SET == t)
	{
		HandleSeq subvardecls;
		HandleSet subvars;      // avoid duplicating variables
		for (const Handle& v : vardecl->getOutgoingSet())
		{
			if (filter_vardecl(v, hs) and subvars.find(v) == subvars.end()) {
				subvardecls.push_back(v);
				subvars.insert(v);
			}
		}
		if (subvardecls.empty() and get_free_variables(hs).empty())
			return Handle::UNDEFINED;
		if (subvardecls.size() == 1)
			return subvardecls[0];
		return createLink(std::move(subvardecls), t);
	}

	// If we're here we have failed to recognize vardecl as a useful
	// and well-formed variable declaration, so Handle::UNDEFINED is
	// returned. XXX FIXME -- surely this should be a throw, instead!!!
	return Handle::UNDEFINED;
}

} // ~namespace opencog
