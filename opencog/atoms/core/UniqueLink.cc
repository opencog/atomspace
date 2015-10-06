/*
 * UniqueLink.cc
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

#include "UniqueLink.h"

using namespace opencog;

void UniqueLink::init(Type type)
{
	// The name must not be used in another definition
	const Handle& alias = _outgoing[0];
	IncomingSet defs = alias->getIncomingSetByType(type);
	for (LinkPtr def : defs)
	{
		if (def->getOutgoingAtom(0) == alias)
		{
			size_t sz = _outgoing.size();
			for (size_t i=1; i<sz; i++)
			{
				if (def->getOutgoingAtom(i) != _outgoing[i])
				{
					throw InvalidParamException(TRACE_INFO,
					      "Already defined: %s\n",
					       alias->toString().c_str());
				}
			}
		}
	}
}

UniqueLink::UniqueLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: Link(UNIQUE_LINK, oset, tv, av)
{
	init(UNIQUE_LINK);
}

UniqueLink::UniqueLink(const Handle& name, const Handle& defn,
                       TruthValuePtr tv, AttentionValuePtr av)
	: Link(UNIQUE_LINK, HandleSeq({name, defn}), tv, av)
{
	init(UNIQUE_LINK);
}

UniqueLink::UniqueLink(Link &l)
	: Link(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, UNIQUE_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a UniqueLink, got %s", tname.c_str());
	}
	init(tscope);
}

Handle UniqueLink::get_unique(const Handle& alias, Type type)
{
	// Get all UniqueLinks associated with that alias. Be aware that
	// the incoming set will also include those UniqueLinks which
	// have the alias in a position other than the first.
	IncomingSet defs = alias->getIncomingSetByType(type);

	// Return the first (supposedly unique) definition
	for (LinkPtr defl : defs)
	{
		if (defl->getOutgoingAtom(0) == alias)
			return defl->getHandle();
	}

	// There is no definition for the alias
	throw InvalidParamException(TRACE_INFO,
	                            "Cannot find defined hypergraph for atom %s",
	                            alias->toString().c_str());
	return Handle();
}

/* ===================== END OF FILE ===================== */
