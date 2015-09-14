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

void FoldLink::init(void) {}

// ===============================================================

// Place the result into the same atomspace we are in.
// XXX this is bad, buggy, uncomfortable, icky: it pollutes
// the atomspace with intermediate results. This needs to
// be fixed somehow. Right now, I don't know how.
#define DO_RETURN(result) { \
	if (not _atomTable) return (result); \
	AtomSpace* as = _atomTable->getAtomSpace(); \
	return as->add_atom(result); \
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
///
/// This routine is pretending to be more general, though, than simply
/// reducing numeric expressions.  It makes the following assumptions
/// and performs the following actions:
///
/// 1) It is always safe to remove knil from a list. That is, knil is
///    always a unit.
/// 2) Two neighboring elements of the same type can always be kons'ed
///    together with each-other.  That is, kons is called on two
///    neighbors that have the same type. That is, this assumes that
///    the list has the associative property, so that neighboring
///    elements can always be cons'ed together.
/// 3) It does not asssume the commutative property.
/// 4) If distributive_type is set, then kons is called when it seems
///    that one neigboring element might distribute into the next.
///    This is vaguely hacky, and is used to implement distributivity
///    of multiplication over addition.
///
/// For something as simple as the above, the code below is
/// annoyingly complicated.  This is certainly not an efficient,
/// effective way to build a computer algebra system.  It works, its
/// just barely good enough for single-variable arithmetic, but that's
/// all.  For general reduction tasks, there are two choices:
///
/// A) Convert atoms to some other CAS format, reduce that, and then
///    convert back to atoms.
///
/// B) Figure out why the atomspace is so ungainly, and fix it so that
///    it is both easy (easier) to use, and also is high-performance.
///
/// Obviously, B) is much harder than A) but is probably more important.
Handle FoldLink::reduce(void)
{
	// The atom table is typically not set when the ctor runs.
	// So fix it up now.
	if (_atomTable)
		knil = _atomTable->getAtomSpace()->add_atom(knil);

	HandleSeq reduct;
	bool did_reduce = false;

	// First, reduce the outgoing set. Loop over the outgoing set,
	// and call reduce on everything reducible.  Remove all occurances
	// of knil, while we are at it.
	for (const Handle& h: _outgoing)
	{
		Type t = h->getType();

		if (classserver().isA(t, FOLD_LINK))
		{
			FoldLinkPtr fff(FoldLinkCast(h));
			// Arghh.  The cast should have been enough, but we currently
			// can't store these in the atomsapce, due to circular shared
			// lib dependencies.
			if (NULL == fff)
				fff = FoldLinkCast(FoldLink::factory(LinkCast(h)));

			Handle redh = fff->reduce();
			if (h != redh)
			{
				did_reduce = true;
				if (redh != knil)
					reduct.push_back(redh);
			}
			else if (h != knil)
				reduct.push_back(h);
			else
				did_reduce = true;
		}
		else if (h != knil)
			reduct.push_back(h);
		else
			did_reduce = true;
	}

	// If it reduced down to one element, we are done.
	size_t osz = reduct.size();
	if (1 == osz)
	{
		if (not did_reduce)
			return getHandle();
		DO_RETURN(reduct[0]);
	}

	// Next, search for two neighboring atoms of the same type.
	// If two atoms of the same type are found, apply kons to them.
	// Also handle the distributive case.
	for (size_t i = 0; i < osz-1; i++)
	{
		const Handle& hi = reduct[i];
		Type it = hi->getType();

		size_t j = i+1;
		const Handle& hj = reduct[j];
		Type jt = hj->getType();

		// Explore two cases.
		// i and j are the same type. Apply kons, and then recurse.
		bool do_kons = (it == jt);

		// If j is (DistType x a) and i is identical to x,
		// then call kons, because kons is distributive.
		do_kons |= (jt == distributive_type and
		            LinkCast(hj)->getOutgoingAtom(0) == hi);

		if (do_kons)
		{
			Handle cons = kons(hi, hj);

			// If there were only two things in total we are done.
			if (2 == osz)
				DO_RETURN(cons);

			HandleSeq rere;
			for (size_t k=0; k < osz; k++)
			{
				if (k < i)
					rere.push_back(reduct[k]);
				else if (k == i)
					rere.push_back(cons);
				else if (j < k)
					rere.push_back(reduct[k]);
			}

			// Create the reduced atom, and recurse.
			// We need to insert it into the atomspace,
			// so that knil gets placed into the atomspace
			// when reduce is called; else the knil
			// compares up above fail.
			Handle foo(createLink(getType(), rere));
			if (_atomTable)
				foo = _atomTable->getAtomSpace()->add_atom(foo);

			FoldLinkPtr flp = FoldLinkCast(foo);

			// Arghh.  The cast should have been enough, but we currently
			// can't store these in the atomsapce, due to circular shared
			// lib dependencies.
			if (NULL == flp)
				flp = FoldLinkCast(FoldLink::factory(LinkCast(foo)));
			DO_RETURN(Handle(flp->reduce()));
		}
	}

	// If nothing reduced, nothing to do.
	if (not did_reduce)
		return getHandle();

	DO_RETURN(Handle(createLink(getType(), reduct)));
}

// ===========================================================
