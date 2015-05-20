/*
 * opencog/atoms/reduct/FoldLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Fold Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Fold Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <limits>

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atoms/NumberNode.h>
#include "FoldLink.h"

using namespace opencog;

FoldLink::FoldLink(const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FunctionLink(FOLD_LINK, oset, tv, av)
{
	init();
}

FoldLink::FoldLink(Type t, const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FunctionLink(t, oset, tv, av)
{
	if (not classserver().isA(t, FOLD_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FoldLink");
	init();
}

FoldLink::FoldLink(Type t, const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FunctionLink(t, a, b, tv, av)
{
	if (not classserver().isA(t, FOLD_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FoldLink");
	init();
}

FoldLink::FoldLink(Link& l)
    : FunctionLink(l)
{
	Type tscope = l.getType();
	if (not classserver().isA(tscope, FOLD_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FoldLink");
	init();
}

void FoldLink::init(void)
{
	knil = Handle::UNDEFINED;
	kons = NULL;
}

/// reduce() -- reduce the expression by summing constants, etc.
///
/// No actual black-box evaluation or execution is performed. Only
/// clearbox reductions are performed.
///
/// Examples: the reduct of (FoldLink (NumberNode 2) (NumberNode 2))
/// is (NumberNode 4) -- its just a constant.
///
/// The reduct of (FoldLink (VariableNode "$x") (NumberNode 0)) is
/// (VariableNode "$x"), because adding zero to anything yeilds the
/// thing itself.
Handle FoldLink::reduce(void)
{
	// Assume that the expression is a mixture of constants and variables.
	// Sum the constants, and eliminate the nils.
	HandleSeq reduct;
	bool did_reduce = false;

	// First, reduce the outgoing set. Loop over the outgoing set,
	// and call reduce on everything reducible.
	for (const Handle& h: _outgoing)
	{
		Type t = h->getType();

		if (classserver().isA(t, FUNCTION_LINK))
		{
			FunctionLinkPtr fff(FunctionLinkCast(h));
			if (NULL == fff)
				fff = createFunctionLink(*LinkCast(h));

			Handle redh = fff->reduce();
			if (h != redh)
			{
				did_reduce = true;
				reduct.push_back(redh);
			}
			else
				reduct.push_back(h);
		}
		else
			reduct.push_back(h);
	}

	// Next, search for atoms of the same type. If two atoms of the same
	// type are found, apply kons to them.
	size_t osz = reduct.size();
	for (size_t i = 0; i< osz; i++)
	{
		const Handle& hi = reduct[i];
		Type it = hi->getType();

		for (size_t j = i+1; j < osz; j++)
		{
			const Handle& hj = reduct[j];
			Type jt = hj->getType();

			// Same type. Apply kons, and then recurse.
			if (it == jt)
			{
				Handle cons = kons(hi, hj);

				HandleSeq rere;
				rere.push_back(cons);
				for (size_t k=0; k < osz; k++)
				{
					if (i == k or j == k) continue;
					rere.push_back(reduct[k]);
				}

				// Create the reduced atom, and recurse.
				FunctionLinkPtr flp = createFunctionLink(getType(), rere);
				return flp->reduce();
			}
		}
	}

	// If nothing reduced, nothing to do.
	if (not did_reduce)
	{
		return getHandle();
	}

	Handle result(createLink(getType(), reduct));

	// Place the result into the same atomspace we are in.
	// XXX this is bad, buggy, uncomfortable, icky: it pollutes
	// the atomspace with intermediate results. This needs to
	// be fixed somehow. Right now, I don't know how.
	if (not _atomTable) return result;

	AtomSpace* as = _atomTable->getAtomSpace();
	return as->addAtom(result);
}

// ===========================================================
