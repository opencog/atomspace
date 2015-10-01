/*
 * DefineLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  May 2015
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

#include <opencog/atomspace/ClassServer.h>

#include "DefineLink.h"

using namespace opencog;

void DefineLink::init(const HandleSeq& oset)
{
	// Must have name and body
	if (2 != oset.size())
		throw InvalidParamException(TRACE_INFO,
			"Expecting name and definition, got size %d", oset.size());

	_alias = oset[0];
	_definition = oset[1];

	// The name must not be used in another definition
	IncomingSet defs = _alias->getIncomingSetByType(DEFINE_LINK);
	for (LinkPtr def : defs)
	{
		if (2 != def->getArity()) continue;
		if (def->getOutgoingAtom(0) == _alias and
		    def->getOutgoingAtom(1) != _definition)
		{
			throw InvalidParamException(TRACE_INFO,
			                            "Cannot define %s\n"
			                            "with definition %s\n"
			                            "as it is already defined as %s",
			                            _alias->toString().c_str(),
			                            _definition->toString().c_str(),
			                            def->toString().c_str());
		}
	}
}

DefineLink::DefineLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: Link(DEFINE_LINK, oset, tv, av)
{
	init(oset);
}

DefineLink::DefineLink(const Handle& name, const Handle& defn,
                       TruthValuePtr tv, AttentionValuePtr av)
	: Link(DEFINE_LINK, HandleSeq({name, defn}), tv, av)
{
	init(getOutgoingSet());
}

DefineLink::DefineLink(Link &l)
	: Link(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, DEFINE_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a DefineLink, got %s", tname.c_str());
	}

	init(l.getOutgoingSet());
}

Handle DefineLink::get_definition(const Handle& alias)
{
	// Get all DefineLinks associated with that alias. Beware that the
	// incoming set will also include those DefineLinks which have the
	// alias as the definition body.
	IncomingSet defs = alias->getIncomingSetByType(DEFINE_LINK);

	// Return the first (supposedly unique) definition
	for (LinkPtr defl : defs)
	{
		DefineLinkPtr def(DefineLinkCast(defl->getHandle()));
		if (def->get_alias() == alias)
			return def->get_definition();
	}

	// There is no definition for the alias
	throw InvalidParamException(TRACE_INFO,
	                            "Cannot find defined hypergraph for atom %s",
	                            alias->toString().c_str());
	return Handle::UNDEFINED;
}

/* ===================== END OF FILE ===================== */
