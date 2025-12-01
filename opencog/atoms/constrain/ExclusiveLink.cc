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

bool ExclusiveLink::bevaluate(AtomSpace* as, bool silent)
{
	// For now, just return true
	return true;
}

DEFINE_LINK_FACTORY(ExclusiveLink, EXCLUSIVE_LINK);

/* ===================== END OF FILE ===================== */
