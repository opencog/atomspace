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
	// Assume that the expression is a mixture of constants and variables.
	// Sum the constants, and eliminate the nils.
	HandleSeq reduct;
	bool did_reduce = false;
	double sum = knild;
	for (const Handle& h: _outgoing)
	{
		Type t = h->getType();
		if (NUMBER_NODE != t and
		    VARIABLE_NODE != t and
		    (not classserver().isA(t, FUNCTION_LINK))
		)
			throw RuntimeException(TRACE_INFO,
				"Don't know how to reduce %s", h->toShortString().c_str());

		Handle redh(h);
		if (classserver().isA(t, FUNCTION_LINK))
		{
			FunctionLinkPtr fff(FunctionLinkCast(h));
			if (NULL == fff)
				fff = createFunctionLink(*LinkCast(h));

			redh = fff->reduce();
			t = redh->getType();
		}

		if (h != redh) did_reduce = true;

		if (NUMBER_NODE == t)
		{
			NumberNodePtr nnn(NumberNodeCast(redh));
			if (NULL == nnn)
				nnn = createNumberNode(*NodeCast(redh));
			sum = konsd(sum, nnn->getValue());
			did_reduce = true;
			continue;
		}
		reduct.push_back(redh);
	}

	// If nothing reduced, nothing to do.
	if (not did_reduce)
	{
		if (1 == _outgoing.size())
			return _outgoing[0];
		return getHandle();
	}

	// If it reduced to just one number:
	if (0 == reduct.size())
	{
		Handle nsum(createNumberNode(sum));
		if (not _atomTable) return nsum;

		AtomSpace* as = _atomTable->getAtomSpace();
		return as->addAtom(nsum);
	}

	// If it didn't sum to nil, then we have to keep it.
	if (knild != sum)
		reduct.push_back(Handle(createNumberNode(sum)));

	// If it reduced to just one thing:	
	if (1 == reduct.size()) return reduct[0];

	Handle result(createLink(getType(), reduct));

	// Place the result into the same atomspace we are in.
	if (not _atomTable) return result;

	AtomSpace* as = _atomTable->getAtomSpace();
	return as->addAtom(result);
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
