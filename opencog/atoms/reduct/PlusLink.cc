/*
 * opencog/atoms/reduct/PlusLink.cc
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
#include "MinusLink.h"
#include "PlusLink.h"
#include "TimesLink.h"

using namespace opencog;

Handle PlusLink::zero;

PlusLink::PlusLink(const HandleSeq& oset, Type t)
    : ArithmeticLink(oset, t)
{
	init();
}

PlusLink::PlusLink(const Handle& a, const Handle& b)
    : ArithmeticLink({a, b}, PLUS_LINK)
{
	init();
}

PlusLink::PlusLink(const Link& l)
    : ArithmeticLink(l)
{
	init();
}

void PlusLink::init(void)
{
	if (nullptr == zero) zero = createNumberNode(0);

	Type tscope = get_type();
	if (not nameserver().isA(tscope, PLUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PlusLink");

	knil = zero;
	_commutative = true;
}

// ============================================================

static inline double get_double(const ValuePtr& pap)
{
	return NumberNodeCast(pap)->get_value();
}

ValuePtr PlusLink::kons(const ValuePtr& fi, const ValuePtr& fj) const
{
	// Try to yank out values, if possible.
	ValuePtr vi(get_value(fi));
	Type vitype = vi->get_type();

	ValuePtr vj(get_value(fj));
	Type vjtype = vj->get_type();

	// Are they numbers?
	if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
	{
		double sum = get_double(vi) + get_double(vj);
		return createNumberNode(sum);
	}

	// If adding zero, just drop the zero.
	if (NUMBER_NODE == vitype and content_eq(HandleCast(vi), zero))
		return vj;
	if (NUMBER_NODE == vjtype and content_eq(HandleCast(vj), zero))
		return vi;

	// Is either one a PlusLink? If so, then flatten.
	if (PLUS_LINK == vitype or PLUS_LINK == vjtype)
	{
		HandleSeq seq;
		// flatten the left
		if (PLUS_LINK == vitype)
		{
			for (const Handle& lhs: HandleCast(vi)->getOutgoingSet())
				seq.push_back(lhs);
		}
		else
		{
			seq.push_back(HandleCast(vi));
		}

		// flatten the right
		if (PLUS_LINK == vjtype)
		{
			for (const Handle& rhs: HandleCast(vj)->getOutgoingSet())
				seq.push_back(rhs);
		}
		else
		{
			seq.push_back(HandleCast(vj));
		}
		Handle foo(createLink(seq, PLUS_LINK));
		PlusLinkPtr ap = PlusLinkCast(foo);
		return ap->delta_reduce();
	}

	// Is fi identical to fj? If so, then replace by 2*fi
	Handle hvi(HandleCast(vi));
	if (hvi and content_eq(hvi, HandleCast(vj)))
	{
		Handle two(createNumberNode("2"));
		return createTimesLink(hvi, two) -> execute();
	}

	// Swap order, to make the Minus handling below easier.
	if (nameserver().isA(vitype, NUMBER_NODE))
	{
		std::swap(vi, vj);
		std::swap(vitype, vjtype);
	}

	// Collapse (3 + (5 - x)) and (13 + (x - 6))
	if (MINUS_LINK == vitype and NUMBER_NODE == vjtype)
	{
		Handle minuend(HandleCast(vi)->getOutgoingAtom(0));
		Handle subtrahend(HandleCast(vi)->getOutgoingAtom(1));
		if (NUMBER_NODE == minuend->get_type())
		{
			double sum = get_double(vj) + get_double(minuend);
			Handle hsum(createNumberNode(sum));
			return createMinusLink(hsum, subtrahend);
		}
		if (NUMBER_NODE == subtrahend->get_type())
		{
			double diff = get_double(vj) - get_double(subtrahend);
			Handle hdiff(createNumberNode(diff));
			if (content_eq(hdiff, zero))
				return minuend;
			return createPlusLink(minuend, hdiff);
		}
	}

	// If j is (TimesLink x a) and i is identical to x,
	// then create (TimesLink x (a+1))
	//
	// If j is (TimesLink x a) and i is (TimesLink x b)
	// then create (TimesLink x (a+b))
	//
	if (vjtype == TIMES_LINK)
	{
		bool do_add = false;
		HandleSeq rest;

		Handle exx = HandleCast(vj)->getOutgoingAtom(0);

		// Handle the (a+1) case described above.
		if (vi == exx)
		{
			Handle one(createNumberNode("1"));
			rest.push_back(one);
			do_add = true;
		}

		// Handle the (a+b) case described above.
		else if (vitype == TIMES_LINK and
		         hvi->getOutgoingAtom(0) == exx)
		{
			const HandleSeq& ilpo = hvi->getOutgoingSet();
			size_t ilpsz = ilpo.size();
			for (size_t k=1; k<ilpsz; k++)
				rest.push_back(ilpo[k]);
			do_add = true;
		}

		if (do_add)
		{
			const HandleSeq& jlpo = HandleCast(vj)->getOutgoingSet();
			size_t jlpsz = jlpo.size();
			for (size_t k=1; k<jlpsz; k++)
				rest.push_back(jlpo[k]);

			// a_plus is now (a+1) or (a+b) as described above.
			Handle foo(createLink(rest, PLUS_LINK));
			PlusLinkPtr ap = PlusLinkCast(foo);
			ValuePtr a_plus(ap->delta_reduce());

			return createTimesLink(exx, HandleCast(a_plus));
		}
	}

	// Swap order, make things easier below.
	if (nameserver().isA(vitype, FLOAT_VALUE))
	{
		std::swap(vi, vj);
		std::swap(vitype, vjtype);
	}

	// Scalar times vector
	if (NUMBER_NODE == vitype and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		return plus(get_double(vi), FloatValueCast(vj));
	}

	// Vector times vector
	if (nameserver().isA(vitype, FLOAT_VALUE) and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		return plus(FloatValueCast(vi), FloatValueCast(vj));
	}

	Handle hi(HandleCast(vi));
	if (nullptr == hi) hi= HandleCast(fi);

	Handle hj(HandleCast(vj));
	if (nullptr == hj) hj= HandleCast(fj);

	// If we are here, we've been asked to add two things of the same
	// type, but they are not of a type that we know how to add.
	// For example, fi and fj might be two different VariableNodes.
	return createPlusLink(hi, hj);
}

DEFINE_LINK_FACTORY(PlusLink, PLUS_LINK);

// ============================================================
