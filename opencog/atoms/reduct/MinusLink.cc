/*
 * opencog/atoms/reduct/MinusLink.cc
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
 * All Rights Reserved
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "MinusLink.h"
#include "NumericFunctionLink.h"
#include "PlusLink.h"

using namespace opencog;

MinusLink::MinusLink(const HandleSeq&& oset, Type t)
    : PlusLink(std::move(oset), t)
{
	init();
}

MinusLink::MinusLink(const Handle& a, const Handle& b)
    : PlusLink({a, b}, MINUS_LINK)
{
	init();
}

void MinusLink::init(void)
{
	Type tscope = get_type();
	if (not nameserver().isA(tscope, MINUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a MinusLink");

	_commutative = false;
	knil = createNumberNode(0);

	// Disallow unary Minus. This makes things easier, overall.
	if (1 == _outgoing.size())
		_outgoing.insert(_outgoing.begin(), HandleCast(knil));
}

ValuePtr MinusLink::kons(AtomSpace* as, bool silent,
                         const ValuePtr& fi, const ValuePtr& fj) const
{
	// Try to yank out values, if possible.
	ValuePtr vi(NumericFunctionLink::get_value(as, silent, fi));
	Type vitype = vi->get_type();

	ValuePtr vj(NumericFunctionLink::get_value(as, silent, fj));
	Type vjtype = vj->get_type();

	// If vj is zero, just drop it.
	if (NUMBER_NODE == vjtype and content_eq(HandleCast(vj), zero))
		return vi;

	// Are they numbers? If so, perform vector (pointwise) subtraction.
	// Always lower the strength: Number+Number->Number
	// but FloatValue+Number->FloatValue
	try
	{
		if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
			return createNumberNode(minus(vi, vj, true));

		return minus(vi, vj, true);
	}
	catch (const SilentException& ex)
	{
		// If we are here, they were not simple numbers.
	}

	// Collapse (3 - (5 + x)) and (3 - (x + 5))
	if (NUMBER_NODE == vitype and PLUS_LINK == vjtype)
	{
		Handle augend(HandleCast(vj)->getOutgoingAtom(0));
		Handle addend(HandleCast(vj)->getOutgoingAtom(1));
		if (NUMBER_NODE == augend->get_type())
		{
			Handle hdiff(createNumberNode(minus(vi, augend)));
			return createMinusLink(hdiff, addend);
		}
		if (NUMBER_NODE == addend->get_type())
		{
			Handle hdiff(createNumberNode(minus(vi, addend)));
			return createMinusLink(hdiff, augend);
		}
	}

	// Collapse ((x + 13) - 6) and ((13 + x) - 6)
	if (PLUS_LINK == vitype and NUMBER_NODE == vjtype)
	{
		Handle augend(HandleCast(vi)->getOutgoingAtom(0));
		Handle addend(HandleCast(vi)->getOutgoingAtom(1));
		if (NUMBER_NODE == augend->get_type())
		{
			Handle hdiff(createNumberNode(minus(augend, vj)));
			if (content_eq(hdiff, zero))
				return addend;
			return createPlusLink(addend, hdiff);
		}
		if (NUMBER_NODE == addend->get_type())
		{
			Handle hdiff(createNumberNode(minus(addend, vj)));
			if (content_eq(hdiff, zero))
				return augend;
			return createPlusLink(augend, hdiff);
		}
	}

	// ------------------------------------------------------------------
	// Values
	try
	{
		if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
			return createNumberNode(minus(vi, vj, true));

		return minus(vi, vj, true);
	}
	catch (const SilentException& ex)
	{
		// If we are here, they were not simple numbers.
	}

	Handle hi(HandleCast(vi));
	if (nullptr == hi) hi = HandleCast(fi);

	Handle hj(HandleCast(vj));
	if (nullptr == hj) hj = HandleCast(fj);

	// If we are here, we've been asked to subtract two things,
	// but they are not of a type that we know how to subtract.
	return createMinusLink(hi, hj);
}

DEFINE_LINK_FACTORY(MinusLink, MINUS_LINK)

// ============================================================
