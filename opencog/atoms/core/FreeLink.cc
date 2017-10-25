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

#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include "FreeLink.h"

using namespace opencog;

FreeLink::FreeLink(const Handle& a)
    : Link(FREE_LINK, a)
{
	init();
}

FreeLink::FreeLink(const HandleSeq& oset, Type t)
    : Link(oset, t)
{
	if (not classserver().isA(t, FREE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FreeLink");

	// Derived classes have thier own init routines.
	if (FREE_LINK != t) return;
	init();
}

FreeLink::FreeLink(Type t, const Handle& a)
    : Link(t, a)
{
	if (not classserver().isA(t, FREE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FreeLink");

	// Derived classes have thier own init routines.
	if (FREE_LINK != t) return;
	init();
}

FreeLink::FreeLink(Type t, const Handle& a, const Handle& b)
    : Link(t, a, b)
{
	if (not classserver().isA(t, FREE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FreeLink");

	// Derived classes have thier own init routines.
	if (FREE_LINK != t) return;
	init();
}

FreeLink::FreeLink(const Link& l)
    : Link(l)
{
	Type tscope = l.get_type();
	if (not classserver().isA(tscope, FREE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FreeLink");

	// Derived classes have thier own init routines.
	if (FREE_LINK != tscope) return;
	init();
}

/* ================================================================= */

void FreeLink::init(void)
{
	_vars.find_variables(_outgoing);
}

DEFINE_LINK_FACTORY(FreeLink, FREE_LINK);

/* ===================== END OF FILE ===================== */
