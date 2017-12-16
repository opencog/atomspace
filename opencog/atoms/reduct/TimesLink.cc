/*
 * opencog/atoms/reduct/TimesLink.cc
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
#include <opencog/atoms/core/NumberNode.h>
#include "TimesLink.h"

using namespace opencog;

TimesLink::TimesLink(const HandleSeq& oset, Type t)
    : ArithmeticLink(oset, t)
{
	init();
}

TimesLink::TimesLink(Type t, const Handle& a, const Handle& b)
    : ArithmeticLink(t, a, b)
{
	init();
}

TimesLink::TimesLink(const Handle& a, const Handle& b)
    : ArithmeticLink(TIMES_LINK, a, b)
{
	init();
}

TimesLink::TimesLink(const Link& l)
    : ArithmeticLink(l)
{
	init();
}

void TimesLink::init(void)
{
	Type tscope = get_type();
	if (not classserver().isA(tscope, TIMES_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a TimesLink");

	knil = Handle(createNumberNode(1));
	_commutative = true;
}

// ============================================================

static inline double get_double(const Handle& h)
{
	return NumberNodeCast(h)->get_value();
}

/// Because there is no ExpLink or PowLink that can handle repeated
/// products, or any distributive property, kons is very simple for
/// the TimesLink.
Handle TimesLink::kons(const Handle& fi, const Handle& fj) const
{
	Type fitype = fi->get_type();
	Type fjtype = fj->get_type();

	// Are they numbers?
	if (NUMBER_NODE == fitype and NUMBER_NODE == fjtype)
	{
		double prod = get_double(fi) * get_double(fj);
		return Handle(createNumberNode(prod));
	}

	// If either one is the unit, then just drop it.
	if (content_eq(fi, knil))
		return fj;
	if (content_eq(fj, knil))
		return fi;

	// Is either one a TimesLink? If so, then flatten.
	if (TIMES_LINK == fitype or TIMES_LINK == fjtype)
	{
		HandleSeq seq;
		// flatten the left
		if (TIMES_LINK == fitype)
		{
			for (const Handle& lhs: fi->getOutgoingSet())
				seq.push_back(lhs);
		}
		else
		{
			seq.push_back(fi);
		}

		// flatten the right
		if (TIMES_LINK == fjtype)
		{
			for (const Handle& rhs: fj->getOutgoingSet())
				seq.push_back(rhs);
		}
		else
		{
			seq.push_back(fj);
		}
		Handle foo(createLink(seq, TIMES_LINK));
		TimesLinkPtr ap = TimesLinkCast(foo);
		return ap->delta_reduce();
	}

	// If we are here, we've been asked to multiply two things of the
	// same type, but they are not of a type that we know how to multiply.
	return Handle(createTimesLink(fi, fj)->reorder());
}

DEFINE_LINK_FACTORY(TimesLink, TIMES_LINK)

// ============================================================
