/*
 * opencog/atoms/reduct/MinusLink.cc
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
#include "MinusLink.h"
#include "PlusLink.h"

using namespace opencog;

MinusLink::MinusLink(const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : PlusLink(MINUS_LINK, oset, tv, av)
{
	init();
}

MinusLink::MinusLink(Type t, const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : PlusLink(t, oset, tv, av)
{
	if (not classserver().isA(t, MINUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a MinusLink");
	init();
}

MinusLink::MinusLink(const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : PlusLink(MINUS_LINK, a, b, tv, av)
{
	init();
}

MinusLink::MinusLink(Type t, const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : PlusLink(t, a, b, tv, av)
{
	if (not classserver().isA(t, MINUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a MinusLink");
	init();
}

MinusLink::MinusLink(Link& l)
    : PlusLink(l)
{
	Type tscope = l.getType();
	if (not classserver().isA(tscope, MINUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a MinusLink");
	init();
}

void MinusLink::init(void)
{
	size_t sz = _outgoing.size();
	if (2 < sz or 0 == sz)
		throw InvalidParamException(TRACE_INFO,
			"Don't know how to subract that!");
}

NumberNodePtr MinusLink::unwrap_set(Handle h) const
{
	// Pattern matching hack. The pattern matcher returns sets of atoms;
	// if that set contains numbers or something numeric, then unwrap it.
	if (SET_LINK == h->getType())
	{
		LinkPtr lp(LinkCast(h));
		if (1 != lp->getArity())
			throw SyntaxException(TRACE_INFO,
				"Don't know how to subtract this: %s", h->toString().c_str());
		h = lp->getOutgoingAtom(0);
	}

	FoldLinkPtr flp(FoldLinkCast(h));
	if (nullptr == flp and classserver().isA(h->getType(), FOLD_LINK))
		flp = FoldLinkCast(FoldLink::factory(LinkCast(h)));
	if (flp)
		h = flp->execute();

	NumberNodePtr na(NumberNodeCast(h));
	if (nullptr == na)
		throw SyntaxException(TRACE_INFO,
			"Don't know how to subtract this: %s", h->toString().c_str());
	return na;
}

Handle MinusLink::execute(AtomSpace* as) const
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

Handle MinusLink::do_execute(const HandleSeq& oset) const
{
	if (1 == oset.size())
	{
		NumberNodePtr na(unwrap_set(oset[0]));
		return createNumberNode(- na->get_value())->getHandle();
	}

	NumberNodePtr na(unwrap_set(oset[0]));
	NumberNodePtr nb(unwrap_set(oset[1]));
	return createNumberNode(na->get_value() - nb->get_value())->getHandle();
}

// ============================================================
