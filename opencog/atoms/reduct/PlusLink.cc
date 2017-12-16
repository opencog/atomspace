/*
 * opencog/atoms/reduct/PlusLink.cc
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

#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include "PlusLink.h"
#include "TimesLink.h"

using namespace opencog;

PlusLink::PlusLink(const HandleSeq& oset, Type t)
    : ArithmeticLink(oset, t)
{
	init();
}

PlusLink::PlusLink(const Handle& a, const Handle& b)
    : ArithmeticLink(PLUS_LINK, a, b)
{
	init();
}

PlusLink::PlusLink(Type t, const Handle& a, const Handle& b)
    : ArithmeticLink(t, a, b)
{
	init();
}

PlusLink::PlusLink(const Link& l)
    : ArithmeticLink(l)
{
	init();
}

void PlusLink::init(void)
{
	Type tscope = get_type();
	if (not classserver().isA(tscope, PLUS_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PlusLink");

	knil = Handle(createNumberNode(0));
	_commutative = true;
}

// ============================================================

static inline double get_double(const Handle& h)
{
	return NumberNodeCast(h)->get_value();
}

Handle PlusLink::kons(const Handle& fi, const Handle& fj) const
{
	Type fitype = fi->get_type();
	Type fjtype = fj->get_type();

	// Are they numbers?
	if (NUMBER_NODE == fitype and NUMBER_NODE == fjtype)
	{
		double sum = get_double(fi) + get_double(fj);
		return Handle(createNumberNode(sum));
	}

	// If either one is the unit, then just drop it.
	if (content_eq(fi, knil))
		return fj;
	if (content_eq(fj, knil))
		return fi;

	// Is either one a PlusLink? If so, then flatten.
	if (PLUS_LINK == fitype or PLUS_LINK == fjtype)
	{
		HandleSeq seq;
		// flatten the left
		if (PLUS_LINK == fitype)
		{
			for (const Handle& lhs: fi->getOutgoingSet())
				seq.push_back(lhs);
		}
		else
		{
			seq.push_back(fi);
		}

		// flatten the right
		if (PLUS_LINK == fjtype)
		{
			for (const Handle& rhs: fj->getOutgoingSet())
				seq.push_back(rhs);
		}
		else
		{
			seq.push_back(fj);
		}
		Handle foo(createLink(seq, PLUS_LINK));
		PlusLinkPtr ap = PlusLinkCast(foo);
		return ap->delta_reduce();
	}

	// Is fi identical to fj? If so, then replace by 2*fi
	if (content_eq(fi, fj))
	{
		Handle two(createNumberNode("2"));
		return Handle(createTimesLink(fi, two));
	}

	// If j is (TimesLink x a) and i is identical to x,
	// then create (TimesLink x (a+1))
	//
	// If j is (TimesLink x a) and i is (TimesLink x b)
	// then create (TimesLink x (a+b))
	//
	if (fjtype == TIMES_LINK)
	{
		bool do_add = false;
		HandleSeq rest;

		Handle exx = fj->getOutgoingAtom(0);

		// Handle the (a+1) case described above.
		if (fi == exx)
		{
			Handle one(createNumberNode("1"));
			rest.push_back(one);
			do_add = true;
		}

		// Handle the (a+b) case described above.
		else if (fitype == TIMES_LINK and
		         fi->getOutgoingAtom(0) == exx)
		{
			const HandleSeq& ilpo = fi->getOutgoingSet();
			size_t ilpsz = ilpo.size();
			for (size_t k=1; k<ilpsz; k++)
				rest.push_back(ilpo[k]);
			do_add = true;
		}

		if (do_add)
		{
			const HandleSeq& jlpo = fj->getOutgoingSet();
			size_t jlpsz = jlpo.size();
			for (size_t k=1; k<jlpsz; k++)
				rest.push_back(jlpo[k]);

			// a_plus is now (a+1) or (a+b) as described above.
			Handle foo(createLink(rest, PLUS_LINK));
			PlusLinkPtr ap = PlusLinkCast(foo);
			Handle a_plus(ap->delta_reduce());

			return Handle(createTimesLink(exx, a_plus));
		}
	}

	// If we are here, we've been asked to add two things of the same
	// type, but they are not of a type that we know how to add.
	// For example, fi and fj might be two different VariableNodes.
	return Handle(createPlusLink(fi, fj)->reorder());
}

DEFINE_LINK_FACTORY(PlusLink, PLUS_LINK);
// ============================================================
