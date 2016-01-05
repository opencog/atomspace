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

void MapLink::init(void)
{
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"MapLink is expected to be arity-2 only!");

	Type tscope = _outgoing[0]->getType();
	if (not classserver().isA(tscope, SCOPE_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw SyntaxException(TRACE_INFO,
			"Expecting a ScopeLink, got %s", tname.c_str());
	}

	_pattern = ScopeLinkCast(_outgoing[0]);
	_vars = &_pattern->get_variables();
	_varset = &_vars->varset;

	FunctionLink::init();
}

MapLink::MapLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(MAP_LINK, oset, tv, av)
{
	init();
}

MapLink::MapLink(const Handle& vars, const Handle& body,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(MAP_LINK, HandleSeq({vars, body}), tv, av)
{
	init();
}

MapLink::MapLink(Type t, const Handle& body,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(t, HandleSeq({body}), tv, av)
{
	// Derived types have a different initialization sequence.
	if (MAP_LINK != t) return;
	init();
}

MapLink::MapLink(Type t, const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(t, oset, tv, av)
{
	// Derived types have a different initialization sequence.
	if (MAP_LINK != t) return;
	init();
}

MapLink::MapLink(Link &l)
	: FunctionLink(l)
{
	// Type must be as expected
	Type tmap = l.getType();
	if (not classserver().isA(tmap, MAP_LINK))
	{
		const std::string& tname = classserver().getTypeName(tmap);
		throw SyntaxException(TRACE_INFO,
			"Expecting a MapLink, got %s", tname.c_str());
	}

	// Derived types have a different initialization sequence.
	if (MAP_LINK != tmap) return;
	init();
}

// ===============================================================

bool MapLink::extract(const Handle& termpat,
                      const Handle& ground,
                      std::map<Handle,Handle>& valmap,
                      AtomSpace* scratch) const
{
	if (termpat == ground) return true;

	Type t = termpat->getType();
	// If its a variable, then see if we know its value already;
	// If not, then record it.
	if (VARIABLE_NODE == t and 0 < _varset->count(termpat))
	{
		auto val = valmap.find(termpat);
		if (valmap.end() != val)
		{
			// If we already have a value, the value must be identical.
			return (val->second == ground);
		}

		// Check the type of the value.
		if (not _vars->is_type(termpat, ground)) return false;

		// If we are here, everything looks good. Record and return.
		valmap.emplace(std::make_pair(termpat, ground));
		return true;
	}

	return false;
}

Handle MapLink::execute(AtomSpace* scratch) const
{
	std::map<Handle,Handle> valmap;

	if (not extract(_pattern->get_body(), _outgoing[1], valmap, scratch)) 
		return Handle::UNDEFINED;

	return Handle::UNDEFINED;
}

/* ===================== END OF FILE ===================== */
