/*
 * ExtractLink.cc
 *
 * Copyright (C) 2015, 2016 Linas Vepstas
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

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/TypeNode.h>
#include <opencog/atoms/core/FreeLink.h>

#include "ExtractLink.h"

using namespace opencog;

ExtractLink::ExtractLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: ScopeLink(EXTRACT_LINK, oset, tv, av)
{
	ScopeLink::init();
}

ExtractLink::ExtractLink(const Handle& vars, const Handle& body,
                       TruthValuePtr tv, AttentionValuePtr av)
	: ScopeLink(EXTRACT_LINK, HandleSeq({vars, body}), tv, av)
{
	ScopeLink::init();
}

ExtractLink::ExtractLink(Type t, const Handle& body,
                       TruthValuePtr tv, AttentionValuePtr av)
	: ScopeLink(t, HandleSeq({body}), tv, av)
{
	// Derived types have a different initialization sequence.
	if (EXTRACT_LINK != t) return;
	ScopeLink::init();
}

ExtractLink::ExtractLink(Type t, const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: ScopeLink(t, oset, tv, av)
{
	// Derived types have a different initialization sequence.
	if (EXTRACT_LINK != t) return;
	ScopeLink::init();
}

ExtractLink::ExtractLink(Link &l)
	: ScopeLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, EXTRACT_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw SyntaxException(TRACE_INFO,
			"Expecting a ExtractLink, got %s", tname.c_str());
	}

	// Derived types have a different initialization sequence.
	if (EXTRACT_LINK != tscope) return;
	ScopeLink::init();
}

// ===============================================================

bool ExtractLink::extract_rec(const Handle& term,
                              std::map<Handle,Handle>& valmap) const
{
	return false;
}

Handle ExtractLink::extract(const Handle& term) const
{
	return Handle::UNDEFINED;
}

/* ===================== END OF FILE ===================== */
