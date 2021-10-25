/*
 * opencog/atoms/reduct/DivideLink.cc
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "DivideLink.h"
#include "NumericFunctionLink.h"

using namespace opencog;

DivideLink::DivideLink(const HandleSeq&& oset, Type t)
    : TimesLink(std::move(oset), t)
{
	init();
}

DivideLink::DivideLink(const Handle& a, const Handle& b)
    : TimesLink({a, b}, DIVIDE_LINK)
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

// No ExpLink or PowLink and so kons is very simple
ValuePtr DivideLink::kons(AtomSpace* as, bool silent,
                          const ValuePtr& fi, const ValuePtr& fj) const
{
	// Try to yank out values, if possible.
	ValuePtr vi(NumericFunctionLink::get_value(as, silent, fi));
	Type vitype = vi->get_type();

	ValuePtr vj(NumericFunctionLink::get_value(as, silent, fj));
	Type vjtype = vj->get_type();

	// Are they numbers? If so, perform vector (pointwise) subtraction.
	// Always lower the strength: Number+Number->Number
	// but FloatValue+Number->FloatValue
	try
	{
		if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
			return createNumberNode(divide(vi, vj, true));

		return divide(vi, vj, true);
	}
	catch (const SilentException& ex)
	{
		// If we are here, they were not simple numbers.
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
			Handle hquot(createNumberNode(divide(vi, multiplier)));
			return createDivideLink(hquot, multiplicand);
		}
		if (NUMBER_NODE == multiplicand->get_type())
		{
			Handle hquot(createNumberNode(divide(vi, multiplicand)));
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
			Handle hquot(createNumberNode(divide(multiplier, vj)));
			if (content_eq(hquot, one))
				return multiplicand;
			return createTimesLink(multiplicand, hquot);
		}
		if (NUMBER_NODE == multiplicand->get_type())
		{
			Handle hquot(createNumberNode(divide(multiplicand, vj)));
			if (content_eq(hquot, one))
				return multiplier;
			return createTimesLink(multiplier, hquot);
		}
	}

	// ------------------------------------------------------------
	// Values 
	try
	{
		if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
			return createNumberNode(divide(vi, vj, true));

		return divide(vi, vj, true);
	}
	catch (const SilentException& ex)
	{
		// If we are here, they were not simple numbers.
	}

	Handle hi(HandleCast(vi));
	if (nullptr == hi) hi = HandleCast(fi);

	Handle hj(HandleCast(vj));
	if (nullptr == hj) hj = HandleCast(fj);

	// If we are here, we've been asked to take a ratio of two things,
	// but they are not of a type that we know how to divide.
	return createDivideLink(hi, hj);
}

DEFINE_LINK_FACTORY(DivideLink, DIVIDE_LINK)

// ============================================================
