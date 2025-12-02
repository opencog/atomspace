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
#include <opencog/atoms/free/FindUtils.h>
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

bool IdenticalLink::is_identical(void) const
{
	// Count number of non-variables:
	size_t nfix = 0;
	for (const Handle& h: _outgoing)
		if (is_closed(h)) nfix ++;

	// We're OK, if there's just zero or one constants in here.
	if (2 > nfix)
		return true;

	// Lets see if they're all the same ...
	Handle id;
	for (const Handle& h: _outgoing)
	{
		if (not is_closed(h)) continue;
		if (nullptr == id)
		{
			id = h;
			continue;
		}

		if (*h != *id)
			return false;
	}
	return true;
}

/* ================================================================= */

void IdenticalLink::setAtomSpace(AtomSpace* as)
{
	if (not is_identical())
		throw SyntaxException(TRACE_INFO,
			"Cannot place IdenticalLink with non-identical atoms in AtomSpace!  Got %s",
			to_string().c_str());

	Link::setAtomSpace(as);
}

/* ================================================================= */

/// Check for syntactic equality. Specifically, when comparing
/// atoms, the handles MUST be the same handle.
/// This can only return false if this Link is not yet in any AtomSpace.
bool IdenticalLink::bevaluate(AtomSpace* as, bool silent)
{
	return is_identical();
}

DEFINE_LINK_FACTORY(IdenticalLink, IDENTICAL_LINK);

/* ===================== END OF FILE ===================== */
