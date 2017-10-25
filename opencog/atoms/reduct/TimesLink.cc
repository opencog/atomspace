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

	knild = 1.0;
	knil = Handle(createNumberNode("1"));
}

// ============================================================

double TimesLink::konsd(double a, double b) const { return a*b; }

static inline double get_double(const Handle& h)
{
	NumberNodePtr nnn(NumberNodeCast(h));
	if (NULL == nnn)
		nnn = createNumberNode(*NodeCast(h));

	return nnn->get_value();
}

/// Because there is no ExpLink or PowLink that can handle repeated
/// products, or any distributive property, kons is very simple for
/// the TimesLink.
Handle TimesLink::kons(const Handle& fi, const Handle& fj)
{
	// Are they numbers?
	if (NUMBER_NODE == fi->get_type() and
	    NUMBER_NODE == fj->get_type())
	{
		double prod = get_double(fi) * get_double(fj);
		return Handle(createNumberNode(prod));
	}

	// If we are here, we've been asked to multiply two things of the
	// same type, but they are not of a type that we know how to multiply.
	return Handle(createTimesLink(fi, fj)->reorder());
}

DEFINE_LINK_FACTORY(TimesLink, TIMES_LINK)

// ============================================================
