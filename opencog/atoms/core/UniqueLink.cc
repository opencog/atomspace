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
#include <opencog/atomspace/AtomSpace.h>

#include "UniqueLink.h"

using namespace opencog;

void UniqueLink::init(bool allow_open)
{
	if (UNIQUE_LINK == _type)
		throw InvalidParamException(TRACE_INFO,
			"UniqueLinks are private and cannot be instantiated.");

	if (not nameserver().isA(_type, UNIQUE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(_type);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a UniqueLink, got %s", tname.c_str());
	}

	FreeLink::init();
}

UniqueLink::UniqueLink(const HandleSeq&& oset, Type type)
	: FreeLink(std::move(oset), type)
{
	// Derived types have their own initialization
	if (UNIQUE_LINK != type) return;
	init(true);
}

UniqueLink::UniqueLink(const Handle& name, const Handle& defn)
	: FreeLink(HandleSeq({name, defn}), UNIQUE_LINK)
{
	init(true);
}

// Force uniqueness at the time of insertion into the AtomSpace.
void UniqueLink::setAtomSpace(AtomSpace* as)
{
	const Handle& alias = _outgoing[0];
	IncomingSet defs = alias->getIncomingSetByType(_type);
	for (const Handle& def : defs)
	{
		if (def->getOutgoingAtom(0) != alias) continue;

		size_t sz = _outgoing.size();
		for (size_t i=1; i<sz; i++)
		{
			if (def->getOutgoingAtom(i) != _outgoing[i])
			{
				throw InvalidParamException(TRACE_INFO,
				      "Already defined: %s\n",
				       alias->to_string().c_str());
			}
		}
	}

	// If we are here, its all OK.
	Link::setAtomSpace(as);
}

/// Get the unique link for this alias. The implementatation here allows
/// UniqueLinks to be hidden by other UniqueLinks in different frames.
/// That is, the association remains unique, per AtomSpace, but deeper
/// unique links can be hidden by shallower ones (that are different).
/// This is particlalry useful for the StateLinks.
Handle UniqueLink::get_unique_nt(const Handle& alias, Type type,
                                 bool disallow_open, const AtomSpace* as)
{
	// Get all UniqueLinks associated with the alias. Be aware that
	// the incoming set will also include those UniqueLinks which
	// have the alias in a position other than the first.
	IncomingSet defs = alias->getIncomingSetByType(type, as);

	// Return the shallowest (supposedly unique) definition that
	// has no variables in it. We look for the shallowest, since
	// it is the one that hides any of the deeper ones, defined
	// in deeper atomspaces.
	Handle shallowest;
	int depth = INT_MAX;
	for (const Handle& defl : defs)
	{
		if (defl->getOutgoingAtom(0) != alias) continue;
		if (disallow_open)
		{
			UniqueLinkPtr ulp(UniqueLinkCast(defl));
			if (0 < ulp->get_vars().varseq.size()) continue;
		}
		int lvl = as->depth(defl);
		if (0 <= lvl and lvl < depth)
		{
			shallowest = defl;
			depth = lvl;
		}
	}
	return shallowest;
}

Handle UniqueLink::get_unique(const Handle& alias, Type type,
                              bool allow_open, const AtomSpace* as)
{
	Handle shallowest(get_unique_nt(alias, type, allow_open, as));

	if (shallowest) return shallowest;

	// There is no definition for the alias.
	throw InvalidParamException(TRACE_INFO,
	                            "Cannot find definition for atom %s",
	                            alias->to_string().c_str());
}

DEFINE_LINK_FACTORY(UniqueLink, UNIQUE_LINK)

/* ===================== END OF FILE ===================== */
