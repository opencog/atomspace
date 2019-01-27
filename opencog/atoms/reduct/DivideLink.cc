/*
 * opencog/atoms/reduct/DivideLink.cc
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
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
#include <opencog/atoms/core/NumberNode.h>
#include "DivideLink.h"

using namespace opencog;

DivideLink::DivideLink(const HandleSeq& oset, Type t)
    : TimesLink(oset, t)
{
	init();
}

DivideLink::DivideLink(const Handle& a, const Handle& b)
    : TimesLink({a, b}, DIVIDE_LINK)
{
	init();
}

DivideLink::DivideLink(const Link& l)
    : TimesLink(l)
{
	init();
}

void DivideLink::init(void)
{
	Type tscope = get_type();
	if (not nameserver().isA(tscope, DIVIDE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a DivideLink");

	_commutative = false;

	// Disallow unary Divide. This makes things easier, overall.
	if (1 == _outgoing.size())
		_outgoing.insert(_outgoing.begin(), one);
}

static inline double get_double(const ValuePtr& pap)
{
	return NumberNodeCast(pap)->get_value();
}

// No ExpLink or PowLink and so kons is very simple
ValuePtr DivideLink::kons(const ValuePtr& fi, const ValuePtr& fj) const
{
	// Try to yank out values, if possible.
	ValuePtr vi(get_value(fi));
	Type vitype = vi->get_type();

	ValuePtr vj(get_value(fj));
	Type vjtype = vj->get_type();

	// Are they numbers?
	if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
	{
		double ratio = get_double(vi) / get_double(vj);
		return Handle(createNumberNode(ratio));
	}

	// If vj is one, just drop it
	if (NUMBER_NODE == vjtype and content_eq(HandleCast(vj), one))
		return vi;

	// Collapse (3 / (5 * x)) and (3 / (x * 5))
	if (NUMBER_NODE == vitype and TIMES_LINK == vjtype)
	{
		Handle multiplier(HandleCast(vj)->getOutgoingAtom(0));
		Handle multiplicand(HandleCast(vj)->getOutgoingAtom(1));
		if (NUMBER_NODE == multiplier->get_type())
		{
			double quot = get_double(vi) / get_double(multiplier);
			Handle hquot(createNumberNode(quot));
			return createDivideLink(hquot, multiplicand);
		}
		if (NUMBER_NODE == multiplicand->get_type())
		{
			double quot = get_double(vi) / get_double(multiplicand);
			Handle hquot(createNumberNode(quot));
			return createDivideLink(hquot, multiplier);
		}
	}

	// Collapse ((x * 13) / 6) and ((13 * x) / 6)
	if (TIMES_LINK == vitype and NUMBER_NODE == vjtype)
	{
		Handle multiplier(HandleCast(vi)->getOutgoingAtom(0));
		Handle multiplicand(HandleCast(vi)->getOutgoingAtom(1));
		if (NUMBER_NODE == multiplier->get_type())
		{
			double quot = get_double(multiplier) / get_double(vj);
			Handle hquot(createNumberNode(quot));
			if (content_eq(hquot, one))
				return multiplicand;
			return createTimesLink(multiplicand, hquot);
		}
		if (NUMBER_NODE == multiplicand->get_type())
		{
			double quot = get_double(multiplicand) / get_double(vj);
			Handle hquot(createNumberNode(quot));
			if (content_eq(hquot, one))
				return multiplier;
			return createTimesLink(multiplier, hquot);
		}
	}

	// ------------------------------------------------------------
	// Values 
	// Scalar divided by vector
	if (NUMBER_NODE == vitype and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		return divide(get_double(vi), FloatValueCast(vj));
	}

	// Vector divided by scalar
	if (nameserver().isA(vitype, FLOAT_VALUE) and NUMBER_NODE == vjtype)
	{
		return times(1.0/get_double(vj), FloatValueCast(vi));
	}

	// Vector divided by vector
	if (nameserver().isA(vitype, FLOAT_VALUE) and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		return divide(FloatValueCast(vi), FloatValueCast(vj));
	}

	Handle hi(HandleCast(vi));
	if (nullptr == hi) hi= HandleCast(fi);

	Handle hj(HandleCast(vj));
	if (nullptr == hj) hj= HandleCast(fj);

	// If we are here, we've been asked to take a ratio of two things,
	// but they are not of a type that we know how to divide.
	return createDivideLink(hi, hj);
}

DEFINE_LINK_FACTORY(DivideLink, DIVIDE_LINK)

// ============================================================
