/*
 * opencog/atoms/constrain/IsClosedLink.cc
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics LLC
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

#include <algorithm>

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/free/FindUtils.h>

#include "IsClosedLink.h"

using namespace opencog;

IsClosedLink::IsClosedLink(const HandleSeq&& oset, Type t)
    : EvaluatableLink(std::move(oset), t)
{
	if (not nameserver().isA(t, IS_CLOSED_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting an IsClosedLink");
}

/* ================================================================= */

/// Return true if all of the outgoing atoms are closed
/// (contain no free variables).
bool IsClosedLink::bevaluate(AtomSpace* as, bool silent)
{
	for (const Handle& h : _outgoing)
	{
		if (h->is_executable())
		{
			ValuePtr vp(h->execute(as, silent));
			if (not vp->is_atom())
				return false;

			if (not is_closed(HandleCast(vp)))
				return false;

			continue;
		}

		if (not is_closed(h))
			return false;
	}

	return true;
}

DEFINE_LINK_FACTORY(IsClosedLink, IS_CLOSED_LINK);

/* ===================== END OF FILE ===================== */
