/*
 * opencog/atoms/reduct/DivideLink.cc
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

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atoms/NumberNode.h>
#include "DivideLink.h"
#include "TimesLink.h"

using namespace opencog;

DivideLink::DivideLink(const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : TimesLink(DIVIDE_LINK, oset, tv, av)
{
	init();
}

DivideLink::DivideLink(Type t, const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : TimesLink(t, oset, tv, av)
{
	if (not classserver().isA(t, DIVIDE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a DivideLink");
	init();
}

DivideLink::DivideLink(const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : TimesLink(DIVIDE_LINK, a, b, tv, av)
{
	init();
}

DivideLink::DivideLink(Type t, const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : TimesLink(t, a, b, tv, av)
{
	if (not classserver().isA(t, DIVIDE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a DivideLink");
	init();
}

DivideLink::DivideLink(Link& l)
    : TimesLink(l)
{
	Type tscope = l.getType();
	if (not classserver().isA(tscope, DIVIDE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a DivideLink");
	init();
}

void DivideLink::init(void)
{
	size_t sz = _outgoing.size();
	if (2 < sz or 0 == sz)
		throw InvalidParamException(TRACE_INFO,
			"Don't know how to divide that!");
}

Handle DivideLink::execute(AtomSpace* as) const
{
	// Pattern matching hack. The pattern matcher returns sets of atoms;
	// if that set contains numbers or something numeric, then unwrap it.
	if (SET_LINK == _type and 1 == _outgoing.size())
	{
		LinkPtr lp(LinkCast(_outgoing[0]));
		return do_execute(lp->getOutgoingSet());
	}
	return do_execute(_outgoing);
}

Handle DivideLink::do_execute(const HandleSeq& oset) const
{
	if (1 == oset.size())
	{
		NumberNodePtr na(unwrap_set(oset[0]));
		return createNumberNode(1.0 / na->get_value())->getHandle();
	}

	NumberNodePtr na(unwrap_set(oset[0]));
	NumberNodePtr nb(unwrap_set(oset[1]));
	return createNumberNode(na->get_value() / nb->get_value())->getHandle();
}

// ============================================================
