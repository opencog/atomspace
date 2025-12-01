/*
 * opencog/atoms/constrain/IdenticalLink.cc
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
#include "IdenticalLink.h"

using namespace opencog;

IdenticalLink::IdenticalLink(const HandleSeq&& oset, Type t)
    : EvaluatableLink(std::move(oset), t)
{
	if (not nameserver().isA(t, IDENTICAL_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting an IdenticalLink");
}

/* ================================================================= */

void IdenticalLink::setAtomSpace(AtomSpace* as)
{
	// Count number of non-variables:
	size_t nfix = 0;
	for (const Handle& h: _outgoing)
		if (not h->is_type(VARIABLE_NODE)) nfix ++;

	// We're OK, if there's just zero or one constants in here.
	if (2 > _outgoing.size() - nfix)
	{
		Link::setAtomSpace(as);
		return;
	}

	// Lets see if they're all the same ...
	Handle id;
	for (const Handle& h: _outgoing)
	{
		if (h->is_type(VARIABLE_NODE)) continue;
		if (nullptr == id)
		{
			id = h;
			continue;
		}

		if (*h != *id)
			throw RuntimeException(TRACE_INFO,
				"Cannot place IdenticalLink with non-identical atoms in AtomSpace!  Got %s",
				to_string().c_str());
	}

	// Not reachable.
	OC_ASSERT(false, "Internal error");
}

/* ================================================================= */

/// Check for syntactic equality. Specifically, when comparing
/// atoms, the handles MUST be the same handle.
bool IdenticalLink::bevaluate(AtomSpace* as, bool silent)
{
	size_t nelts = _outgoing.size();
	if (2 > nelts) return true;

	for (size_t j=1; j<nelts; j++)
	{
		if (_outgoing[0] != _outgoing[j]) return false;
	}
	return true;
}

DEFINE_LINK_FACTORY(IdenticalLink, IDENTICAL_LINK);

/* ===================== END OF FILE ===================== */
