/*
 * opencog/atoms/reduct/PlusLink.cc
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "MinusLink.h"
#include "NumericOutLink.h"
#include "PlusLink.h"
#include "TimesLink.h"

using namespace opencog;

Handle PlusLink::zero;

PlusLink::PlusLink(const HandleSeq&& oset, Type t)
    : ArithmeticLink(std::move(oset), t)
{
	init();
}

PlusLink::PlusLink(const Handle& a, const Handle& b)
    : ArithmeticLink({a, b}, PLUS_LINK)
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

ValuePtr PlusLink::kons(AtomSpace* as, bool silent,
                        const ValuePtr& fi, const ValuePtr& fj) const
{
	if (fj == knil)
		return NumericOutLink::get_value(as, silent, fi);

	// Try to yank out values, if possible.
	ValuePtr vi(NumericOutLink::get_value(as, silent, fi));
	Type vitype = vi->get_type();

	ValuePtr vj(fj);
	Type vjtype = vj->get_type();

	// If adding zero, just drop the zero.
	if (NUMBER_NODE == vjtype and content_eq(HandleCast(vj), zero))
		return vi;

	// Are they numbers? If so, perform vector (pointwise) addition.
	// Always lower the strength: Number+Number->Number
	// but FloatValue+Number->FloatValue
	try
	{
		if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
			return createNumberNode(plus(vi, vj, true));

		return plus(vi, vj, true);
	}
	catch (const SilentException& ex)
	{
		// If we are here, they were not simple numbers.
	}

	// Is vi a PlusLink? If so, then flatten.
	if (PLUS_LINK == vitype)
	{
		HandleSeq seq = HandleCast(vi)->getOutgoingSet();

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
		PlusLinkPtr ap = createPlusLink(std::move(seq));
		return ap->delta_reduce(as, silent);
	}

	if (PLUS_LINK == vjtype)
	{
		// Paste on one at a time; this avoids what would otherwise
		// be infinite recursion on `(Plus A B C)` where kons was
		// unable to reduce `(Plus B C)`. So we instead try to do
		// `(Plus (Plus A B) C)` which should work out...
		ValuePtr vsum = vi;
		for (const Handle& h : HandleCast(vj)->getOutgoingSet())
		{
			if (PLUS_LINK == vsum->get_type())
			{
				HandleSeq vout(HandleCast(vsum)->getOutgoingSet());
				vout.push_back(h);
				vsum = createPlusLink(std::move(vout));
			}
			else
			{
				vsum = kons(as, silent, vsum, h);
			}
		}
		return vsum;
	}

	// Is vi identical to vj? If so, then replace by 2*vi
	Handle hvi(HandleCast(vi));
	if (hvi and content_eq(hvi, HandleCast(vj)))
	{
		Handle two(createNumberNode("2"));
		return createTimesLink(hvi, two) -> execute(as, silent);
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
			Handle hsum(createNumberNode(plus(vj, minuend)));
			return createMinusLink(hsum, subtrahend);
		}
		if (NUMBER_NODE == subtrahend->get_type())
		{
			Handle hdiff(createNumberNode(
				minus(vj, subtrahend)));
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
			Handle foo(createLink(std::move(rest), PLUS_LINK));
			PlusLinkPtr ap = PlusLinkCast(foo);
			ValuePtr a_plus(ap->delta_reduce(as, silent));

			return createTimesLink(exx, HandleCast(a_plus));
		}
	}

	// Swap order, make things easier below.
	if (nameserver().isA(vitype, FLOAT_VALUE))
	{
		std::swap(vi, vj);
		std::swap(vitype, vjtype);
	}

	// vector ops...
	if (NUMBER_NODE == vitype and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		return plus(NumberNodeCast(vi), FloatValueCast(vj));
	}

	// Vector plus vector
	if (nameserver().isA(vitype, FLOAT_VALUE) and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		return plus(FloatValueCast(vi), FloatValueCast(vj));
	}

	Handle hi(HandleCast(vi));
	if (nullptr == hi) hi = HandleCast(fi);

	Handle hj(HandleCast(vj));
	if (nullptr == hj) hj = HandleCast(fj);

	// If we are here, we've been asked to add two things of the same
	// type, but they are not of a type that we know how to add.
	// For example, fi and fj might be two different VariableNodes.
	return createPlusLink(hi, hj);
}

DEFINE_LINK_FACTORY(PlusLink, PLUS_LINK);

// ============================================================
