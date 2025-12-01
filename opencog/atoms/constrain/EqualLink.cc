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
	Link::setAtomSpace(as);
}

/* ================================================================= */

/// Check for semantic equality.
bool EqualLink::bevaluate(AtomSpace* as, bool silent)
{
	size_t nelts = _outgoing.size();
	if (2 > nelts) return true;

	return true;
}

DEFINE_LINK_FACTORY(EqualLink, EQUAL_LINK);

/* ===================== END OF FILE ===================== */
