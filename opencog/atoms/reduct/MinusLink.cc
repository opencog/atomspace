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
    : ArithmeticLink(oset, t)
{
	init();
}

MinusLink::MinusLink(const Handle& a, const Handle& b)
    : ArithmeticLink({a, b}, MINUS_LINK)
{
	init();
}

MinusLink::MinusLink(const Link& l)
    : ArithmeticLink(l)
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

static inline double get_double(const Handle& h)
{
	return NumberNodeCast(h)->get_value();
}

ProtoAtomPtr MinusLink::kons(const Handle& fi, const ProtoAtomPtr& fj) const
{
	// Are they numbers?
	if (NUMBER_NODE == fi->get_type() and
	    NUMBER_NODE == fj->get_type())
	{
		double diff = get_double(fi) - get_double(HandleCast(fj));
		return createNumberNode(diff);
	}

	// If fj is zero, just drop it.
	if (content_eq(HandleCast(fj), HandleCast(knil)))
		return fi;

	// If we are here, we've been asked to subtract two things,
	// but they are not of a type that we know how to subtract.
	Handle sand(HandleCast(fj));
	if (nullptr == sand)
		throw SyntaxException(TRACE_INFO, "Not aan Atom!");
	return Handle(createMinusLink(fi, sand));
}

DEFINE_LINK_FACTORY(MinusLink, MINUS_LINK)

// ============================================================
