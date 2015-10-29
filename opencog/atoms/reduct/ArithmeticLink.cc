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

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atoms/NumberNode.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/execution/Instantiator.h>
#include "ArithmeticLink.h"

using namespace opencog;

ArithmeticLink::ArithmeticLink(const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FoldLink(ARITHMETIC_LINK, oset, tv, av)
{
	init();
}

ArithmeticLink::ArithmeticLink(Type t, const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FoldLink(t, oset, tv, av)
{
	if (not classserver().isA(t, ARITHMETIC_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a ArithmeticLink");
	init();
}

ArithmeticLink::ArithmeticLink(Type t, const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FoldLink(t, a, b, tv, av)
{
	if (not classserver().isA(t, ARITHMETIC_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a ArithmeticLink");
	init();
}

ArithmeticLink::ArithmeticLink(Link& l)
    : FoldLink(l)
{
	Type tscope = l.getType();
	if (not classserver().isA(tscope, ARITHMETIC_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a ArithmeticLink");
	init();
}

void ArithmeticLink::init(void)
{
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
		if (h->getType() == VARIABLE_NODE)
			vars.push_back(h);
		else if (h->getType() == NUMBER_NODE)
			numbers.push_back(h);
		else
			exprs.push_back(h);
	}

	HandleSeq result;
	for (const Handle& h : vars) result.push_back(h);
	for (const Handle& h : exprs) result.push_back(h);
	for (const Handle& h : numbers) result.push_back(h);

	Handle h(FoldLink::factory(getType(), result));
	if (NULL == _atomTable) return h;

	return _atomTable->getAtomSpace()->add_atom(h);
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
	if (nnn == NULL)
		throw RuntimeException(TRACE_INFO,
			  "Expecting a NumberNode, got %s",
		     classserver().getTypeName(h->getType()).c_str());

	return nnn->get_value();
}

NumberNodePtr ArithmeticLink::unwrap_set(Handle h) const
{
	// Pattern matching hack. The pattern matcher returns sets of atoms;
	// if that set contains numbers or something numeric, then unwrap it.
	if (SET_LINK == h->getType())
	{
		LinkPtr lp(LinkCast(h));
		if (1 != lp->getArity())
			throw SyntaxException(TRACE_INFO,
				"Don't know how to do arithmetic with this: %s",
				h->toString().c_str());
		h = lp->getOutgoingAtom(0);
	}

	if (DEFINED_SCHEMA_NODE == h->getType())
	{
		h = DefineLink::get_definition(h);
	}

	FunctionLinkPtr flp(FunctionLinkCast(h));
	if (nullptr == flp and classserver().isA(h->getType(), FUNCTION_LINK))
		flp = FunctionLinkCast(FunctionLink::factory(LinkCast(h)));
	if (flp)
		h = flp->execute();

	NumberNodePtr na(NumberNodeCast(h));
	if (nullptr == na)
		throw SyntaxException(TRACE_INFO,
			"Don't know how to do arithmetic with this: %s",
			h->toString().c_str());
	return na;
}

Handle ArithmeticLink::execute(AtomSpace* as) const
{
	// Pattern matching hack. The pattern matcher returns sets of atoms;
	// if that set contains numbers or something numeric, then unwrap it.
	if (SET_LINK == _type and 1 == _outgoing.size())
	{
		LinkPtr lp(LinkCast(_outgoing[0]));
		return do_execute(as, lp->getOutgoingSet());
	}
	return do_execute(as, _outgoing);
}

Handle ArithmeticLink::do_execute(AtomSpace* as, const HandleSeq& oset) const
{
	// XXX FIXME, we really want the instantiator to do the work
	// here, but there is a giant circular-shared-library mess
	// that results if we do this. So i'm disabling for now.
#ifdef CIRCULAR_SHARED_LIBS
	Instantiator inst(as);
#endif
	double sum = knild;
	for (Handle h: oset)
	{
#ifdef CIRCULAR_SHARED_LIBS
		h = inst.execute(h);
#else
		h = unwrap_set(h);
#endif
		sum = konsd(sum, get_double(as, h));
	}

	if (as) return as->add_atom(createNumberNode(sum));
	return Handle(createNumberNode(sum));
}
// ===========================================================
