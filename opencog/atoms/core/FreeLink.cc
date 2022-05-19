/*
 * opencog/atoms/core/FreeLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
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
#include <opencog/atoms/core/Quotation.h>
#include "FreeLink.h"

using namespace opencog;

FreeLink::FreeLink(const HandleSeq&& oset, Type t)
    : Link(std::move(oset), t)
{
	if (FREE_LINK == t)
		throw InvalidParamException(TRACE_INFO,
			"FreeLinks are private and cannot be instantiated.");

	if (not nameserver().isA(t, FREE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FreeLink");

	// Derived classes have their own init routines.
	if (FREE_LINK != t) return;
	init();
}

/* ================================================================= */

void FreeLink::init(void)
{
	if (unquoted_below(_outgoing)) return;
	_vars.find_variables(_outgoing, true);
}

DEFINE_LINK_FACTORY(FreeLink, FREE_LINK);

/* ===================== END OF FILE ===================== */
