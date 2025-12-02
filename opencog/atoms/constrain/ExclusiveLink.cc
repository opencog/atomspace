/*
 * opencog/atoms/constrain/ExclusiveLink.cc
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
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include "ExclusiveLink.h"

#include <algorithm>

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
	// The EvaluatableLink will have sorted the outgoing set for us.
	// If there are two identical atoms in here, they will appear next
	// to each other. Do NOT allow such links to be placed into the
	// AtomSpace.
	//
	// One exception to this rule is an executable link -- it might
	// appear twice, but, when executed, return different results
	// each time it is run.
	for (size_t i = 0; i < _outgoing.size()-1; i++)
	{
		if (_outgoing[i]->is_executable()) continue;

		if (*_outgoing[i] == *_outgoing[i+1])
			throw SyntaxException(TRACE_INFO,
				"All members of an ExclusiveLink must differ. Got %s",
				to_string().c_str());
	}

	// If we are here, then yes, all elements differ from one-another.
	Link::setAtomSpace(as);
}

/* ================================================================= */

/// Execute the outgoing set, make sure all results differ.
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

		// If the return value is a ContainerValue, we assume that this
		// is the result of executing a MeetLink or QueryLink.
		// In this case, unwrap it, to get the "actual value".
		// This feels slightly hacky, but will do for just right now.
		if (vp->is_type(CONTAINER_VALUE))
		{
			HandleSeq hs(LinkValueCast(vp)->to_handle_seq());
			if (1 == hs.size())
				vp = hs[0];
			else
				throw RuntimeException(TRACE_INFO,
					"Expecting only one result, got %s",
					vp->to_string().c_str());
		}

		// XXX Not clear what the deal here is. We expect an Atom.
		// Should we throw? Show we ignore? Perhaps throw, for now,
		// until we find out what the "typical user" is trying to do.
		if (not vp->is_atom())
			throw RuntimeException(TRACE_INFO,
				"Expecting execution to return an Atom; got %s\n",
				vp->to_string().c_str());

		exset.emplace_back(as->add_atom(HandleCast(vp)));
	}

	// Sort. If there are two atoms that are the same, they will
	// be sorted so that they are next to each-other.
	std::sort(exset.begin(), exset.end(), content_based_handle_less());

	for (size_t i = 0; i < exset.size()-1; i++)
	{
		if (*exset[i] == *exset[i+1]) return false;
	}

	return true;
}

DEFINE_LINK_FACTORY(ExclusiveLink, EXCLUSIVE_LINK);

/* ===================== END OF FILE ===================== */
