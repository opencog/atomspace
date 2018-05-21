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

#include <opencog/atoms/proto/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include "FreeLink.h"

using namespace opencog;

FreeLink::FreeLink(const HandleSeq& oset, Type t)
    : Link(oset, t)
{
	if (not nameserver().isA(t, FREE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FreeLink");

	unorder();

	// Derived classes have thier own init routines.
	if (FREE_LINK != t) return;
	init();
}

FreeLink::FreeLink(const Link& l)
    : Link(l)
{
	Type tscope = l.get_type();
	if (not nameserver().isA(tscope, FREE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FreeLink");

	unorder();

	// Derived classes have thier own init routines.
	if (FREE_LINK != tscope) return;
	init();
}

/* ================================================================= */

void FreeLink::unorder(void)
{
	if (not nameserver().isA(get_type(), UNORDERED_LINK)) return;

	// Place into arbitrary, but deterministic order.
	// We have to do this here,  because some links,
	// e.g. EqualLink and IdenticalLink are unordered,
	// but don't inherit from the C++ UnorderedLink class.
	// So we hack around and do it here.
	std::sort(_outgoing.begin(), _outgoing.end(), handle_less());
}

void FreeLink::init(void)
{
	_vars.find_variables(_outgoing);
}

DEFINE_LINK_FACTORY(FreeLink, FREE_LINK);

/* ===================== END OF FILE ===================== */
