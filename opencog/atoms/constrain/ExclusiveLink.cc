/*
 * opencog/atoms/constrain/ExclusiveLink.cc
 *
 * Copyright (C) 2024 BrainyBlaze Dynamics, LLC
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
#include <opencog/atomspace/AtomSpace.h>
#include "ExclusiveLink.h"

using namespace opencog;

ExclusiveLink::ExclusiveLink(const HandleSeq&& oset, Type t)
    : EvaluatableLink(std::move(oset), t)
{
	if (not nameserver().isA(t, EXCLUSIVE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting an ExclusiveLink");
}

/* ================================================================= */

void ExclusiveLink::setAtomSpace(AtomSpace* as)
{
	Link::setAtomSpace(as);
}

/* ================================================================= */

/// Check to make sure all atoms differ
bool ExclusiveLink::bevaluate(AtomSpace* as, bool silent)
{
	HandleSeq exset;
	exset.reserve(_outgoing.size());

	for (const Handle& h: _outgoing)
	{
		if (not h->is_executable())
		{
			exset.push_back(h);
			continue;
		}
		ValuePtr vp(h->execute(as, silent));

#if NOT_NEEDED_YET
		// Uhh ... !!??

		// If the return value is a ContainerValue, we assume that this
		// is the result of executing a MeetLink or QueryLink.
		// In this case, unwrap it, to get the "actual value".
		// This feels slightly hacky, but will do for just right now.
		if (vp->is_type(CONTAINER_VALUE))
		{
			HandleSeq hs(LinkValueCast(vp)->to_handle_seq());
			if (1 == hs.size())
				vp = hs[0];
		}
#endif

		// XXX Not clear what the deal here is. We expect an Atom.
		// Should we throw? Show we ignore? Perhaps throw, for now,
		// until we find out what the "typical user" is trying to do.
		if (not vp->is_atom())
			throw RuntimeExcetion(TRACE_INFO,
				"Expecting execution to return an Atom; got %s\n",
				vp->to_string().c_str());

		exset.emplace_back(as->add_atom(HandleCast(vp)));
	}

	return true;
}

DEFINE_LINK_FACTORY(ExclusiveLink, EXCLUSIVE_LINK);

/* ===================== END OF FILE ===================== */
