/*
 * opencog/atoms/reduct/TimesLink.cc
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

#include <opencog/atoms/proto/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "TimesLink.h"

using namespace opencog;

Handle TimesLink::one;

TimesLink::TimesLink(const HandleSeq& oset, Type t)
    : ArithmeticLink(oset, t)
{
	init();
}

TimesLink::TimesLink(const Handle& a, const Handle& b)
    : ArithmeticLink({a,b}, TIMES_LINK)
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
	if (nullptr == one) one = createNumberNode(1);
	Type tscope = get_type();
	if (not nameserver().isA(tscope, TIMES_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a TimesLink");

	knil = one;
	_commutative = true;
}

// ============================================================

static inline double get_double(const ProtoAtomPtr& pap)
{
	return NumberNodeCast(pap)->get_value();
}

/// Because there is no ExpLink or PowLink that can handle repeated
/// products, or any distributive property, kons is very simple for
/// the TimesLink.
ProtoAtomPtr TimesLink::kons(const ProtoAtomPtr& fi, const ProtoAtomPtr& fj) const
{
	Type fitype = fi->get_type();
	Type fjtype = fj->get_type();

	// Are they both numbers?
	if (NUMBER_NODE == fitype and NUMBER_NODE == fjtype)
	{
		double prod = get_double(fi) * get_double(fj);
		return createNumberNode(prod);
	}

	// If either one is the unit, then just drop it.
	if (content_eq(HandleCast(fi), one))
		return fj;
	if (content_eq(HandleCast(fj), one))
		return fi;

	// Is either one a TimesLink? If so, then flatten.
	if (TIMES_LINK == fitype or TIMES_LINK == fjtype)
	{
		Handle hi(HandleCast(fi));
		HandleSeq seq;
		// flatten the left
		if (TIMES_LINK == fitype)
		{
			for (const Handle& lhs: hi->getOutgoingSet())
				seq.push_back(lhs);
		}
		else
		{
			seq.push_back(hi);
		}

		// flatten the right
		if (TIMES_LINK == fjtype)
		{
			for (const Handle& rhs: HandleCast(fj)->getOutgoingSet())
				seq.push_back(rhs);
		}
		else
		{
			seq.push_back(HandleCast(fj));
		}
		Handle foo(createLink(seq, TIMES_LINK));
		TimesLinkPtr ap = TimesLinkCast(foo);
		return ap->delta_reduce();
	}

	// Try to yank out values, if possible.
	ProtoAtomPtr vi(fi);
	if (nameserver().isA(fitype, VALUE_OF_LINK))
	{
		vi = FunctionLinkCast(fi)->execute();
	}
	Type vitype = vi->get_type();

	ProtoAtomPtr vj(fj);
	if (nameserver().isA(fjtype, VALUE_OF_LINK))
	{
		vj = FunctionLinkCast(fj)->execute();
	}
	Type vjtype = vj->get_type();

	// Swap order, make things easier below.
	if (nameserver().isA(vitype, FLOAT_VALUE))
	{
		std::swap(vi, vj);
		std::swap(vitype, vjtype);
	}

	// Scalar times vector
	if (NUMBER_NODE == vitype and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		return times(get_double(vi), FloatValueCast(vj));
	}

	// Vector times vector
	if (nameserver().isA(vitype, FLOAT_VALUE) and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		return times(FloatValueCast(vi), FloatValueCast(vj));
	}

	// If we are here, we've been asked to multiply two things of the
	// same type, but they are not of a type that we know how to multiply.
	return createTimesLink(HandleCast(fi), HandleCast(fj))->reorder();
}

DEFINE_LINK_FACTORY(TimesLink, TIMES_LINK)

// ============================================================
