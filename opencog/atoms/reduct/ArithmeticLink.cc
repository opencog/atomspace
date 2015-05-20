/*
 * opencog/atoms/reduct/ArithmeticLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Arithmetic Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Arithmetic Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <limits>

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atoms/NumberNode.h>
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
	konsd = NULL;
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
	Handle red(FoldLink::reduce());
	ArithmeticLinkPtr alp(ArithmeticLinkCast(red));
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
/// last, all number nodes (of which there should be only zero or one.)
/// We do not currently sort the variables, but maybe we should...?
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

	if (1 < numbers.size())
		throw RuntimeException(TRACE_INFO,
		      "Expecting the plus link to have already been reduced!");

	HandleSeq result;
	for (const Handle& h : vars) result.push_back(h);
	for (const Handle& h : exprs) result.push_back(h);
	for (const Handle& h : numbers) result.push_back(h);

	Handle h(FunctionLink::factory(getType(), result));
	if (NULL == _atomTable) return h;

	AtomSpace* as = _atomTable->getAtomSpace();
	return as->addAtom(h);
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
	// Recurse, and execute anything below...
	FunctionLinkPtr flp(FunctionLinkCast(h));
	if (flp)
		h = flp->execute(as);

	NumberNodePtr nnn(NumberNodeCast(h));
	if (nnn == NULL)
		throw RuntimeException(TRACE_INFO,
			  "Expecting a NumberNode, got %s",
		     classserver().getTypeName(h->getType()).c_str());

	return nnn->getValue();
}

Handle ArithmeticLink::execute(AtomSpace* as) const
{
	double sum = knild;
	for (Handle h: _outgoing)
	{
		sum = konsd(sum, get_double(as, h));
	}

	if (as) return as->addAtom(createNumberNode(sum));
	return Handle(createNumberNode(sum));
}
// ===========================================================
