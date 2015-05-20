/*
 * opencog/atoms/reduct/PlusLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Plus Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Plus Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atoms/NumberNode.h>
#include "PlusLink.h"
#include "TimesLink.h"

using namespace opencog;

PlusLink::PlusLink(const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : ArithmeticLink(PLUS_LINK, oset, tv, av)
{
	init();
}

PlusLink::PlusLink(Type t, const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : ArithmeticLink(t, oset, tv, av)
{
	if (not classserver().isA(t, PLUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PlusLink");
	init();
}

PlusLink::PlusLink(Type t, const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : ArithmeticLink(t, a, b, tv, av)
{
	if (not classserver().isA(t, PLUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PlusLink");
	init();
}

PlusLink::PlusLink(Link& l)
    : ArithmeticLink(l)
{
	Type tscope = l.getType();
	if (not classserver().isA(tscope, PLUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PlusLink");
	init();
}

static double plus(double a, double b) { return a+b; }

void PlusLink::init(void)
{
	knild = 0.0;
	konsd = plus;
}

// ============================================================

/// re-order the contents of a PlusLink into "lexicographic" order.
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
Handle PlusLink::reorder(void)
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

	return Handle(createPlusLink(result));
}

// ============================================================

/// Handle normalization of addition into multiplication.
/// aka "mutiplicattive reduction"
///
/// There are four cases handled here:
/// x+x ==> 2x
/// x + ax ==> (a+1) x
/// ax + x ==> (a+1) x
/// ax + bx ==> (a + b) x
///
Handle PlusLink::reduce(void)
{
	// First, let FoldLink do its stuff.
	Handle fold = ArithmeticLink::reduce();

	if (PLUS_LINK != fold->getType()) return fold;

	PlusLinkPtr pfold(PlusLinkCast(fold));
	if (NULL == pfold)
		pfold = createPlusLink(*LinkCast(fold));
	fold = pfold->reorder();

	// Now, look for repeated atoms, two atoms that appear twice
	// in the outgoing set. If they do, then can be mutliplied.
	LinkPtr lfold(LinkCast(fold));

	bool do_reduce = false;
	Handle reduct = Handle::UNDEFINED;

	const HandleSeq& ofs = lfold->getOutgoingSet();
	size_t fsz = ofs.size();
	for (size_t i = 0; i < fsz; i++)
	{
		const Handle& fi = ofs[i];
		for (size_t j=i+1; j < fsz; j++)
		{
			const Handle& fj = ofs[j];

			// Is atom in position i identical to atom in position j?
			// If so, then replace by 2*i
			if (fi == fj)
			{
				Handle two(createNumberNode("2"));
				reduct = Handle(createTimesLink(fi, two));
				do_reduce = true;
			}

			// If j is (TimesLink x a) and i is identical to x,
			// then create (TimesLink x (a+1))
			//
			// If j is (TimesLink x a) and i is (TimesLink x b)
			// then create (TimesLink x (a+b))
			//
			else if (fj->getType() == TIMES_LINK)
			{
				bool do_add = false;
				HandleSeq rest;

				LinkPtr ilp = LinkCast(fi);
				LinkPtr jlp = LinkCast(fj);
				Handle exx = jlp->getOutgoingAtom(0);

				// Handle the (a+1) case described above.
				if (fi == exx)
				{
					Handle one(createNumberNode("1"));
					rest.push_back(one);
					do_add = true;
				}

				// Handle the (a+b) case described above.
				else if (ilp->getOutgoingAtom(0) == exx)
				{
					const HandleSeq& ilpo = ilp->getOutgoingSet();
					size_t ilpsz = ilpo.size();
					for (size_t k=1; k<ilpsz; k++)
						rest.push_back(ilpo[k]);
					do_add = true;
				}

				if (do_add)
				{
					const HandleSeq& jlpo = jlp->getOutgoingSet();
					size_t jlpsz = jlpo.size();
					for (size_t k=1; k<jlpsz; k++)
						rest.push_back(jlpo[k]);

					// a_plus is now (a+1) or (a+b) as described above.
					PlusLinkPtr ap = createPlusLink(rest);
					Handle a_plus(ap->reduce());

					reduct = Handle(createTimesLink(exx, a_plus));
					do_reduce = true;
				}
			}

			if (do_reduce)
			{
				HandleSeq norm;
				norm.push_back(reduct);

				// copy everything else, except for i and j.
				for (size_t k = 0; k< fsz; k++)
				{
					if (k == i or k == j) continue;
					norm.push_back(ofs[k]);
				}

				PlusLinkPtr plp = createPlusLink(norm);

				Handle red(plp->reduce());

				// Place the result into the same atomspace we are in.
				// XXX this is bad, buggy, uncomfortable, icky: it pollutes
				// the atomspace with intermediate results. This needs to
				// be fixed somehow. Right now, I don't know how.
				if (not _atomTable) return red;

				AtomSpace* as = _atomTable->getAtomSpace();
				return as->addAtom(red);
			}
		}
	}

	return fold;
}
