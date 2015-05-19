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
    : FoldLink(PLUS_LINK, oset, tv, av)
{
	init();
}

PlusLink::PlusLink(Type t, const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FoldLink(t, oset, tv, av)
{
	if (not classserver().isA(t, PLUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PlusLink");
	init();
}

PlusLink::PlusLink(Type t, const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FoldLink(t, a, b, tv, av)
{
	if (not classserver().isA(t, PLUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PlusLink");
	init();
}

PlusLink::PlusLink(Link& l)
    : FoldLink(l)
{
	Type tscope = l.getType();
	if (not classserver().isA(tscope, PLUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PlusLink");
	init();
}

static double plus(double a, double b) { return a+b; }

void PlusLink::init(void)
{
	knil = 0.0;
	kons = plus;
}


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
	Handle fold = FoldLink::reduce();

	// Now, look for repeated atoms, two atoms that appear twice
	// in the outgoing set. If they do, then can be mutliplied.
	LinkPtr lfold(LinkCast(fold));

	const HandleSeq& ofs = lfold->getOutgoingSet();
	size_t fsz = ofs.size();
	for (size_t i = 0; i < fsz; i++)
	{
		const Handle& fi = ofs[i];
		for (size_t j=i+1; j < fsz; j++)
		{
			if (fi == ofs[j])
			{
				Handle two(createNumberNode("2"));
				Handle tlp(createTimesLink(two, fi));

				HandleSeq norm;
				norm.push_back(tlp);

				// copy everything else, except for i and j.
				for (size_t k = 0; k< fsz; k++)
				{
					if (k == i or k == j) continue;
					norm.push_back(ofs[k]);
				}

				PlusLinkPtr plp = createPlusLink(norm);

				Handle red(plp->reduce());

				// Place the result into the same atomspace we are in.
				if (_atomTable)
				{
					AtomSpace* as = _atomTable->getAtomSpace();
					return as->addAtom(red);
				}
				return red;
			}
		}
	}

	return fold;
}
