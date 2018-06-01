/*
 * opencog/atoms/reduct/MinusLink.cc
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
#include "MinusLink.h"

using namespace opencog;

MinusLink::MinusLink(const HandleSeq& oset, Type t)
    : PlusLink(oset, t)
{
	init();
}

MinusLink::MinusLink(const Handle& a, const Handle& b)
    : PlusLink({a, b}, MINUS_LINK)
{
	init();
}

MinusLink::MinusLink(const Link& l)
    : PlusLink(l)
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

static inline double get_double(const ProtoAtomPtr& pap)
{
	return NumberNodeCast(pap)->get_value();
}

ProtoAtomPtr MinusLink::kons(const ProtoAtomPtr& fi, const ProtoAtomPtr& fj) const
{
	// Try to yank out values, if possible.
	ProtoAtomPtr vi(get_value(fi));
	Type vitype = vi->get_type();

	ProtoAtomPtr vj(get_value(fj));
	Type vjtype = vj->get_type();

	// Are they numbers?
	if (NUMBER_NODE == vitype and NUMBER_NODE == vjtype)
	{
		double diff = get_double(vi) - get_double(vj);
		return createNumberNode(diff);
	}

	// If vj is zero, just drop it.
	if (NUMBER_NODE == vjtype and content_eq(HandleCast(vj), zero))
		return vi;

	// Scalar minus vector
	if (NUMBER_NODE == vitype and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		FloatValuePtr mj = FloatValueCast(times(-1.0, FloatValueCast(vj)));
		return plus(get_double(vi), mj);
	}

	// Vector minus scalar
	if (nameserver().isA(vitype, FLOAT_VALUE) and NUMBER_NODE == vjtype)
	{
		return plus(-get_double(vj), FloatValueCast(vi));
	}

	// Vector times vector
	if (nameserver().isA(vitype, FLOAT_VALUE) and nameserver().isA(vjtype, FLOAT_VALUE))
	{
		FloatValuePtr mj = FloatValueCast(times(-1.0, FloatValueCast(vj)));
		return plus(FloatValueCast(vi), mj);
	}

	Handle hi(HandleCast(vi));
	if (nullptr == hi) hi= HandleCast(fi);

	Handle hj(HandleCast(vj));
	if (nullptr == hj) hj= HandleCast(fj);

	// If we are here, we've been asked to subtract two things,
	// but they are not of a type that we know how to subtract.
	return createMinusLink(hi, hj);
}

DEFINE_LINK_FACTORY(MinusLink, MINUS_LINK)

// ============================================================
