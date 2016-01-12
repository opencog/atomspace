/*
 * RandomNumber.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/util/mt19937ar.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/NumberNode.h>

#include "RandomNumber.h"

using namespace opencog;

static MT19937RandGen randy(616432);

void RandomNumberLink::init()
{
	if (_outgoing.size() != 2)
		throw SyntaxException(TRACE_INFO,
			"Expecting a numerical min and max; got %s",
			toString().c_str());
}

RandomNumberLink::RandomNumberLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: FunctionLink(RANDOM_NUMBER_LINK, oset, tv, av)
{
	init();
}

RandomNumberLink::RandomNumberLink(Link &l)
	: FunctionLink(l)
{
	// Type must be as expected
	Type tscope = l.getType();
	if (not classserver().isA(tscope, RANDOM_NUMBER_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an RandomNumberLink, got %s", tname.c_str());
	}
	init();
}

// ---------------------------------------------------------------

// Pattern matching hack. The pattern matcher returns sets of atoms;
// if that set contains numbers or something numeric, then unwrap it.
static NumberNodePtr unwrap_set(Handle h)
{
	if (SET_LINK == h->getType())
	{
		LinkPtr lp(LinkCast(h));
		if (1 != lp->getArity())
			throw SyntaxException(TRACE_INFO,
				"Don't know how to do arithmetic with this: %s",
				h->toString().c_str());
		h = lp->getOutgoingAtom(0);
	}

	NumberNodePtr na(NumberNodeCast(h));
	if (nullptr == na)
		throw SyntaxException(TRACE_INFO,
			"Don't know how to do arithmetic with this: %s",
			h->toString().c_str());
	return na;
}


Handle RandomNumberLink::execute(AtomSpace * as) const
{
	NumberNodePtr nmin(unwrap_set(_outgoing[0]));
	NumberNodePtr nmax(unwrap_set(_outgoing[1]));

	double cept = nmin->get_value();
	double slope = nmax->get_value() - cept;

	double ary = slope * randy.randdouble() + cept;

	// XXX This is probably wrong ... if the as is null, we should
	// probably use the atomspace that this link is in, right?
	// We need to make a decision here and in many other places...
	// We should probably be doing "lazy-add-to-atomsapce" ...
	if (NULL == as)
		return Handle(createNumberNode(ary));

	return as->add_atom(createNumberNode(ary));
}

/* ===================== END OF FILE ===================== */
