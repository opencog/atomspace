/*
 * opencog/atoms/constrain/EqualLink.cc
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, LLC
 * All Rights Reserved
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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include "EqualLink.h"

using namespace opencog;

EqualLink::EqualLink(const HandleSeq&& oset, Type t)
    : EvaluatableLink(std::move(oset), t)
{
	if (not nameserver().isA(t, EQUAL_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting an EqualLink");
}

/* ================================================================= */

void EqualLink::setAtomSpace(AtomSpace* as)
{
	Handle id;
	for (const Handle& h: _outgoing)
	{
		if (h->is_type(VARIABLE_NODE)) continue;
		if (h->is_executable()) continue;
		if (nullptr == id)
		{
			id = h;
			continue;
		}

		if (*h != *id)
			throw RuntimeException(TRACE_INFO,
				"Cannot placeEqualLink with non-equal elements in the AtomSpace!  Got %s",
				to_string().c_str());
	}

	Link::setAtomSpace(as);
}

/* ================================================================= */

/// Check for semantic equality. -- Are things equal, after execution?
bool EqualLink::bevaluate(AtomSpace* as, bool silent)
{
	size_t nelts = _outgoing.size();
	if (2 > nelts) return true;

	Handle id;
	for (const Handle& h: _outgoing)
	{
		if (h->is_type(VARIABLE_NODE)) continue;

		if (not h->is_executable())
		{
			if (nullptr == id)
			{
				id = h;
				continue;
			}

			if (*h != *id)
				return false;

			continue;
		}

		ValuePtr vp(h->execute(as, silent));
		if (not vp->is_atom())
			throw RuntimeException(TRACE_INFO,
				"Expecting an Atom, got %s\n", vp->to_string().c_str());

		Handle nh(as->add_atom(HandleCast(vp)));

		if (nullptr == id)
		{
			id = nh;
			continue;
		}

		if (*nh != *id)
			return false;
	}
	return true;
}

DEFINE_LINK_FACTORY(EqualLink, EQUAL_LINK);

/* ===================== END OF FILE ===================== */
