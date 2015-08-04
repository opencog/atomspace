/*
 * DefineLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License
 * along with this program; if not, write to:
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

	// The name must not have been previously defined before.
	IncomingSet defs = _alias->getIncomingSetByType(DEFINE_LINK);
	for (LinkPtr def : defs)
        if (def->isSource(_alias))
	        throw InvalidParamException(TRACE_INFO,
	                                    "This is already defined; "
	                                    "remove before redfining!");

	_definition = oset[1];
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

Handle DefineLink::get_definition(const Handle& alias) {
	// Get all DefineLinks associated with that alias, beware that it
	// will also return DefineLink with that alias as definition body.
    IncomingSet defs = alias->getIncomingSetByType(DEFINE_LINK);

    // Return the first (supposedly unique) definition
    for (LinkPtr defl : defs) {
	    DefineLinkPtr def(DefineLinkCast(defl->getHandle()));
	    if (def->get_alias() == alias)
		    return def->get_definition();
    }

    // There is no definition for that alias
    throw InvalidParamException(TRACE_INFO,
                                "Cannot find defined hypergraph for atom %s",
                                alias->toString().c_str());
    return Handle::UNDEFINED;
}

/* ===================== END OF FILE ===================== */
