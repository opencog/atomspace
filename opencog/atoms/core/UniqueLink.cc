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

#include <opencog/atoms/base/ClassServer.h>

#include "UniqueLink.h"

using namespace opencog;

void UniqueLink::init(bool allow_open)
{
	if (allow_open)
	{
		FreeLink::init();

		// The name must not be used in another definition,
		// but only if it has no free variables in the definition.
		// That is, "closed sentences" must be unique.
		if (0 < _vars.varseq.size()) return;
	}

	const Handle& alias = _outgoing[0];
	IncomingSet defs = alias->getIncomingSetByType(_type);
	for (const LinkPtr& def : defs)
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

UniqueLink::UniqueLink(Type type, const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FreeLink(type, oset, tv, av)
{
	if (not classserver().isA(type, UNIQUE_LINK))
	{
		const std::string& tname = classserver().getTypeName(type);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a UniqueLink, got %s", tname.c_str());
	}

	// Derived types have thier own initialization
	if (UNIQUE_LINK != type) return;
	init(true);
}

UniqueLink::UniqueLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FreeLink(UNIQUE_LINK, oset, tv, av)
{
	init(true);
}

UniqueLink::UniqueLink(const Handle& name, const Handle& defn,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FreeLink(UNIQUE_LINK, HandleSeq({name, defn}), tv, av)
{
	init(true);
}

UniqueLink::UniqueLink(Link &l)
	: FreeLink(l)
{
	// Type must be as expected
	Type type = l.getType();
	if (not classserver().isA(type, UNIQUE_LINK))
	{
		const std::string& tname = classserver().getTypeName(type);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a UniqueLink, got %s", tname.c_str());
	}

	// Derived types have thier own initialization
	if (UNIQUE_LINK != type) return;
	init(true);
}

/// Get the unique link for this alias.
Handle UniqueLink::get_unique(const Handle& alias, Type type,
                              bool allow_open)
{
	// Get all UniqueLinks associated with the alias. Be aware that
	// the incoming set will also include those UniqueLinks which
	// have the alias in a position other than the first.
	IncomingSet defs = alias->getIncomingSetByType(type);

	// Return the first (supposedly unique) definition that has no
	// variables in it.
	for (const LinkPtr& defl : defs)
	{
		if (defl->getOutgoingAtom(0) == alias)
		{
			if (allow_open)
			{
				UniqueLinkPtr ulp(UniqueLinkCast(defl));
				if (0 < ulp->get_vars().varseq.size()) continue;
			}
			return defl->getHandle();
		}
	}

	// There is no definition for the alias.
	throw InvalidParamException(TRACE_INFO,
	                            "Cannot find defined hypergraph for atom %s",
	                            alias->toString().c_str());
}

/* ===================== END OF FILE ===================== */
