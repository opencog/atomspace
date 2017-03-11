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
#include "StateLink.h"
#include "TypedAtomLink.h"
#include "UniqueLink.h"

using namespace opencog;

FreeLink::FreeLink(const HandleSeq& oset,
                   TruthValuePtr tv)
    : Link(FREE_LINK, oset, tv)
{
	init();
}

FreeLink::FreeLink(const Handle& a,
                   TruthValuePtr tv)
    : Link(FREE_LINK, a, tv)
{
	init();
}

FreeLink::FreeLink(Type t, const HandleSeq& oset,
                   TruthValuePtr tv)
    : Link(t, oset, tv)
{
	if (not classserver().isA(t, FREE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FreeLink");

	// Derived classes have thier own init routines.
	if (FREE_LINK != t) return;
	init();
}

FreeLink::FreeLink(Type t, const Handle& a,
                   TruthValuePtr tv)
    : Link(t, a, tv)
{
	if (not classserver().isA(t, FREE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FreeLink");

	// Derived classes have thier own init routines.
	if (FREE_LINK != t) return;
	init();
}

FreeLink::FreeLink(Type t, const Handle& a, const Handle& b,
                   TruthValuePtr tv)
    : Link(t, a, b, tv)
{
	if (not classserver().isA(t, FREE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FreeLink");

	// Derived classes have thier own init routines.
	if (FREE_LINK != t) return;
	init();
}

FreeLink::FreeLink(Link& l)
    : Link(l)
{
	Type tscope = l.getType();
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

/* ================================================================= */

FreeLinkPtr FreeLink::factory(const Handle& h)
{
	// If h is of the right form already, its just a matter of calling
	// it.  Otherwise, we have to create
	FreeLinkPtr flp(FreeLinkCast(h));
	if (flp) return flp;

	if (nullptr == h)
		throw RuntimeException(TRACE_INFO, "Not executable!");

	return factory(h->getType(), h->getOutgoingSet());
}

// Basic type factory.
FreeLinkPtr FreeLink::factory(Type t, const HandleSeq& seq)
{
	if (STATE_LINK == t)
		return createStateLink(seq);

	if (TYPED_ATOM_LINK == t)
		return createTypedAtomLink(seq);

	if (UNIQUE_LINK == t)
		return createUniqueLink(seq);

	if (classserver().isA(t, FREE_LINK))
		return createFreeLink(t, seq);

	throw SyntaxException(TRACE_INFO,
		"FreeLink is not a factory for %s",
		classserver().getTypeName(t).c_str());
}
