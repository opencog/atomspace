/*
 * opencog/atoms/core/MapLink.cc
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

#include "MapLink.h"

using namespace opencog;

MapLink::MapLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(MAP_LINK, oset, tv, av)
{
	FunctionLink::init();
}

MapLink::MapLink(const Handle& vars, const Handle& body,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(MAP_LINK, HandleSeq({vars, body}), tv, av)
{
	FunctionLink::init();
}

MapLink::MapLink(Type t, const Handle& body,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(t, HandleSeq({body}), tv, av)
{
	// Derived types have a different initialization sequence.
	if (MAP_LINK != t) return;
	FunctionLink::init();
}

MapLink::MapLink(Type t, const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(t, oset, tv, av)
{
	// Derived types have a different initialization sequence.
	if (MAP_LINK != t) return;
	FunctionLink::init();
}

MapLink::MapLink(Link &l)
	: FunctionLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, MAP_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw SyntaxException(TRACE_INFO,
			"Expecting a MapLink, got %s", tname.c_str());
	}

	// Derived types have a different initialization sequence.
	if (MAP_LINK != tscope) return;
	FunctionLink::init();
}

// ===============================================================

bool MapLink::extract(const Handle& pattern,
                      const Handle& ground,
                      std::map<Handle,Handle>& valmap) const
{
	// if 
	return false;
}

Handle MapLink::execute(AtomSpace* scratch) const
{
	std::map<Handle,Handle> valmap;

	return Handle::UNDEFINED;
}

/* ===================== END OF FILE ===================== */
