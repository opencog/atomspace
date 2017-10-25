/*
 * opencog/atoms/reduct/ArithmeticLink.cc
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

#include <limits>

#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "ArithmeticLink.h"

using namespace opencog;

ArithmeticLink::ArithmeticLink(const HandleSeq& oset, Type t)
    : FoldLink(oset, t)
{
	init();
}

ArithmeticLink::ArithmeticLink(Type t, const Handle& a, const Handle& b)
    : FoldLink(t, a, b)
{
	init();
}

ArithmeticLink::ArithmeticLink(const Link& l)
    : FoldLink(l)
{
	init();
}

void ArithmeticLink::init(void)
{
	Type tscope = get_type();
	if (not classserver().isA(tscope, ARITHMETIC_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a ArithmeticLink");

	knild = std::numeric_limits<double>::quiet_NaN();
}

// ===========================================================
/// reduce() -- reduce the expression by summing constants, etc.
///
/// No actual black-box evaluation or execution is performed. Only
/// clearbox reductions are performed.
///
/// Examples: the reduct of (PlusLink (NumberNode 2) (NumberNode 2))
/// is (NumberNode 4) -- its just a constant.
///
/// The reduct of (PlusLink (VariableNode "$x") (NumberNode 0)) is
/// (VariableNode "$x"), because adding zero to anything yeilds the
/// thing itself.
Handle ArithmeticLink::reduce(void)
{
	Handle road(reorder());
	ArithmeticLinkPtr alp(ArithmeticLinkCast(road));

	Handle red(alp->FoldLink::reduce());

	alp = ArithmeticLinkCast(red);
	if (NULL == alp) return red;
	return alp->reorder();
}

// ============================================================

/// re-order the contents of an ArithmeticLink into "lexicographic" order.
///
/// The goal of the re-ordering is to simplify the reduction code,
/// by placing atoms where they are easily found.  For now, this
/// means:
/// first, all of the variables,
/// next, all compound expressions,
/// last, all number nodes
/// We do not currently sort the variables, but maybe we should...?
/// Sorting by variable names would hold consilidate them...
/// The FoldLink::reduce() method already returns expressions that are
/// almost in the correct order.
Handle ArithmeticLink::reorder(void)
{
	HandleSeq vars;
	HandleSeq exprs;
	HandleSeq numbers;

	for (const Handle& h : _outgoing)
	{
		if (h->get_type() == VARIABLE_NODE)
			vars.push_back(h);
		else if (h->get_type() == NUMBER_NODE)
			numbers.push_back(h);
		else
			exprs.push_back(h);
	}

	HandleSeq result;
	for (const Handle& h : vars) result.push_back(h);
	for (const Handle& h : exprs) result.push_back(h);
	for (const Handle& h : numbers) result.push_back(h);

	Handle h(createLink(result, get_type()));
	if (NULL == _atom_space) return h;

	return _atom_space->add_atom(h);
}

// ===========================================================

/// execute() -- Execute the expression, returning a number
///
/// Similar to reduce(), above, except that this can only work
/// on fully grounded (closed) sentences: after executation,
/// everything must be a number, and there can be no variables
/// in sight.
static inline double get_double(AtomSpace *as, Handle h)
{
	NumberNodePtr nnn(NumberNodeCast(h));
	if (nnn == nullptr)
		throw RuntimeException(TRACE_INFO,
			  "Expecting a NumberNode, got %s",
		     classserver().getTypeName(h->get_type()).c_str());

	return nnn->get_value();
}

NumberNodePtr ArithmeticLink::unwrap_set(Handle h) const
{
	FunctionLinkPtr flp(FunctionLinkCast(h));
	if (flp) h = flp->execute();

	// Pattern matching hack. The pattern matcher returns sets of atoms;
	// if that set contains numbers or something numeric, then unwrap it.
	if (SET_LINK == h->get_type())
	{
		if (1 != h->get_arity())
			throw SyntaxException(TRACE_INFO,
				"Don't know how to do arithmetic with this: %s",
				h->to_string().c_str());
		h = h->getOutgoingAtom(0);
	}

	NumberNodePtr na(NumberNodeCast(h));
	if (nullptr == na)
		throw SyntaxException(TRACE_INFO,
			"Don't know how to do arithmetic with this: %s",
			h->to_string().c_str());
	return na;
}

Handle ArithmeticLink::execute(AtomSpace* as) const
{
	// Pattern matching hack. The pattern matcher returns sets of atoms;
	// if that set contains numbers or something numeric, then unwrap it.
	if (1 == _outgoing.size())
	{
		Handle arg = _outgoing[0];
		FunctionLinkPtr flp(FunctionLinkCast(arg));
		if (flp) arg = flp->execute(as);

		if (SET_LINK == arg->get_type())
		{
			return do_execute(as, arg->getOutgoingSet());
		}
		HandleSeq o;
		o.emplace_back(arg);
		return do_execute(as, o);
	}
	return do_execute(as, _outgoing);
}

Handle ArithmeticLink::do_execute(AtomSpace* as, const HandleSeq& oset) const
{
	double sum = knild;
	for (Handle h: oset)
	{
		h = unwrap_set(h);
		sum = konsd(sum, get_double(as, h));
	}

	if (as) return as->add_atom(createNumberNode(sum));
	return Handle(createNumberNode(sum));
}

// ===========================================================
