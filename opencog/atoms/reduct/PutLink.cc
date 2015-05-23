/*
 * opencog/atoms/reduct/PutLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Put Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Put Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/ClassServer.h>
#include "PutLink.h"

using namespace opencog;

PutLink::PutLink(const HandleSeq& oset,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : FreeLink(PUT_LINK, oset, tv, av)
{
	init();
}

PutLink::PutLink(const Handle& a,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : FreeLink(PUT_LINK, a, tv, av)
{
	init();
}

PutLink::PutLink(Type t, const HandleSeq& oset,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : FreeLink(t, oset, tv, av)
{
	if (not classserver().isA(t, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

PutLink::PutLink(Type t, const Handle& a,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : FreeLink(t, a, tv, av)
{
	if (not classserver().isA(t, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

PutLink::PutLink(Type t, const Handle& a, const Handle& b,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : FreeLink(t, a, b, tv, av)
{
	if (not classserver().isA(t, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

PutLink::PutLink(Link& l)
    : FreeLink(l)
{
	Type tscope = l.getType();
	if (not classserver().isA(tscope, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

// PutLink expects a very strict format: an arity-2 link, with
// the first part being a pattern, and the second a list or set
// of values. If the pattern has N variables, then the seccond
// part must have N values.  The following formats are understood:
//
//    PutLink
//       <pattern with 1 variable>
//       <any single atom>
//
//    PutLink
//       <pattern with N variables>
//       ListLink     ;; must have arity N
//          <atom 1>
//          ...
//          <atom N>
//
// The below is a handy-dandy easy-to-use form. When it is reduced,
// it will result in the creation of a set of reduced forms, not
// just one (the two sets haveing the same arity). Unfortunately,
// this trick cannot work for N=1.
//
//    PutLink
//       <pattern with N variables>
//       SetLink        ;; Must hold a set of ListLinks
//          ListLink    ;; must have arity N
//             <atom 1>
//             ...
//             <atom N>
//
void PutLink::init(void)
{
	if (2 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO, "PutLinks should be arity 2!");

	const Handle& body = _outgoing[0];
	if (VARIABLE_NODE == body->getType())
	{
		_free_vars.push_back(body);
	}
	else
	{
		LinkPtr lll(LinkCast(body));
		if (lll)
		{
			std::set<Handle> varset;
			find_vars(varset, lll->getOutgoingSet());
		}
	}

	// OK, now for the values.
	if (_free_vars.size() == 1) return;

	LinkPtr lval(LinkCast(_outgoing[1]));
	if (lval->getType() == LIST_LINK)
	{
		if (lval->getArity() != _free_vars.size())
			throw InvalidParamException(TRACE_INFO,
				"PutLink has mismatched size! Expected %zu, got %zu\n",
				_free_vars.size(), lval->getArity());
		return;
	}
	if (lval->getType() != SET_LINK)
		throw InvalidParamException(TRACE_INFO,
			"PutLink was expecting a ListLink or SetLink!");

	for (const Handle& h : lval->getOutgoingSet())
	{
		LinkPtr lse(LinkCast(h));
		if (lse->getType() != LIST_LINK)
			throw InvalidParamException(TRACE_INFO,
				"PutLink was expecting a ListLink here");
		if (lse->getArity() != _free_vars.size())
			throw InvalidParamException(TRACE_INFO,
				"PutLink set element has mismatched size! Expected %zu, got %zu\n",
				_free_vars.size(), lse->getArity());
	}
}

Handle PutLink::reduce(void)
{
   throw RuntimeException(TRACE_INFO, "Not reducible!");
}
