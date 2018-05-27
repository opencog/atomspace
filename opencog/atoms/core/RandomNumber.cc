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

#include <opencog/atoms/core/NumberNode.h>

#include "RandomNumber.h"

using namespace opencog;

static MT19937RandGen randy(616432);

void RandomNumberLink::init()
{
	// Type must be as expected
	Type tscope = get_type();
	if (not nameserver().isA(tscope, RANDOM_NUMBER_LINK))
	{
		const std::string& tname = nameserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an RandomNumberLink, got %s", tname.c_str());
	}

	if (_outgoing.size() != 2)
		throw SyntaxException(TRACE_INFO,
			"Expecting a numerical min and max; got %s",
			to_string().c_str());
}

RandomNumberLink::RandomNumberLink(const HandleSeq& oset, Type t)
	: FunctionLink(oset, t)
{
	init();
}

RandomNumberLink::RandomNumberLink(const Link &l)
	: FunctionLink(l)
{
	init();
}

// ---------------------------------------------------------------

// Pattern matching hack. The pattern matcher returns sets of atoms;
// if that set contains numbers or something numeric, then unwrap it.
static NumberNodePtr unwrap_set(Handle h)
{
	if (SET_LINK == h->get_type())
	{
		if (0 == h->get_arity())
			throw SyntaxException(TRACE_INFO,
				"Expecting a number, got the empty set!\n");
		if (1 != h->get_arity())
			throw SyntaxException(TRACE_INFO,
				"Expecting only one number, got more than that: %s",
				h->to_string().c_str());
		h = h->getOutgoingAtom(0);
	}

	NumberNodePtr na(NumberNodeCast(h));
	if (nullptr == na)
		throw SyntaxException(TRACE_INFO,
			"Expecting a number, got this: %s",
			h->to_string().c_str());
	return na;
}


ProtoAtomPtr RandomNumberLink::execute() const
{
	// XXX FIXME so that this also works with values.
	NumberNodePtr nmin(unwrap_set(_outgoing[0]));
	NumberNodePtr nmax(unwrap_set(_outgoing[1]));

	double cept = nmin->get_value();
	double slope = nmax->get_value() - cept;

	double ary = slope * randy.randdouble() + cept;

	return ProtoAtomPtr(createNumberNode(ary));
}

DEFINE_LINK_FACTORY(RandomNumberLink, RANDOM_NUMBER_LINK);

/* ===================== END OF FILE ===================== */
