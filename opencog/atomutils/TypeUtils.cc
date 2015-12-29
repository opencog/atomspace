/*
 * TypeUtils.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  December 2015
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/Link.h>
#include <opencog/atomspace/ClassServer.h>

#include <opencog/atoms/TypeNode.h>
#include <opencog/atoms/core/DefineLink.h>

#include "TypeUtils.h"

using namespace opencog;


/* ================================================================= */
/**
 * Type checker.  Returns true if `val` is of type `deep`.
 */
bool opencog::value_is_type(const Handle& spec, const Handle& val)
{
	Handle deep(spec);

	Type valtype = val->getType();
	Type dpt = deep->getType();

	// If it's a user-defined type, replace by it's defintion.
	if (DEFINED_TYPE_NODE == dpt)
	{
		deep = DefineLink::get_definition(deep);
		dpt = deep->getType();
	}

	// If it's a signature, unpack it now.
	if (SIGNATURE_LINK == dpt)
	{
		LinkPtr dptr(LinkCast(deep));
		deep = dptr->getOutgoingAtom(0);
		dpt = deep->getType();
	}

	if (TYPE_NODE == dpt)
	{
		Type deeptype = TypeNodeCast(deep)->get_value();
		return (valtype == deeptype);
	}
	else if (TYPE_CHOICE == dpt)
	{
		LinkPtr dptr(LinkCast(deep));
		for (const Handle& choice : dptr->getOutgoingSet())
		{
			if (value_is_type(choice, val)) return true;
		}
		return false;
	}
	else if (FUZZY_LINK == dpt)
	{
		throw RuntimeException(TRACE_INFO,
			"Not implemented! TODO XXX FIXME");
	}

	// If it is a node, not a link, then it is a type-constant,
	// and thus must match perfectly.
	LinkPtr dptr(LinkCast(deep));
	if (nullptr == dptr)
		return (deep == val);

	// If a link, then both must be same link type.
	if (valtype != dpt) return false;

	LinkPtr vptr(LinkCast(val));
	const HandleSeq& vlo = vptr->getOutgoingSet();
	const HandleSeq& dpo = dptr->getOutgoingSet();
	size_t sz = dpo.size();

	// Both must be the same size...
	if (vlo.size() != sz) return false;

	// Unordered links are harder to handle...
	if (classserver().isA(dpt, UNORDERED_LINK))
		throw RuntimeException(TRACE_INFO,
			"Not implemented! TODO XXX FIXME");

	// Ordered links are compared side-by-side
	for (size_t i=0; i<sz; i++)
	{
		if (not value_is_type(dpo[i], vlo[i])) return false;
	}

	// If we are here, all checks must hav passed.
	return true;
}

/* ================================================================= */

static bool type_match_rec(const Handle& left_, const Handle& right_, bool toplevel)
{
	Handle left(left_);
	Type ltype = left->getType();

	// If it's a user-defined type, replace by it's defintion.
	if (DEFINED_TYPE_NODE == ltype)
	{
		left = DefineLink::get_definition(left);
		ltype = left->getType();
	}

	// Unpack the arrow; right must match left's input.
	if (ARROW_LINK == ltype)
	{
		LinkPtr larrow(LinkCast(left));
		left = larrow->getOutgoingAtom(0); // 0 == input
	}

	// If right is not a type, then just use value-check.
	Type rtype = right_->getType();
	if (toplevel and
	    TYPE_NODE != rtype and
	    TYPE_CHOICE != rtype and
	    SIGNATURE_LINK != rtype and
	    DEFINED_TYPE_NODE != rtype and
	    ARROW_LINK != rtype)
	{
		return value_is_type(left, right_);
	}

	Handle right(right_);

	// If it's a user-defined type, replace by it's defintion.
	if (DEFINED_TYPE_NODE == rtype)
	{
		right = DefineLink::get_definition(right);
		rtype = right->getType();
	}

	// Unpack the arrow; right's output must match left.
	if (ARROW_LINK == rtype)
	{
		LinkPtr rarrow(LinkCast(right));
		right = rarrow->getOutgoingAtom(1); // 1 == output
	}

	// Should be safe to unpack signatures now.
	// We must not do this earlier, it will mess up.
	if (SIGNATURE_LINK == ltype)
	{
		left = LinkCast(left)->getOutgoingAtom(0);
		ltype = left->getType();
	}

	if (SIGNATURE_LINK == rtype)
	{
		right = LinkCast(right)->getOutgoingAtom(0);
		rtype = right->getType();
	}

	// If left is a core type, right must be that type
	// (or a value, partly handled that above already).
	if (TYPE_NODE == ltype)
	{
		if (left == right) return true;
		return TypeNodeCast(left)->get_value() == rtype;
	}

	// If left is a type choice, right must match a choice.
	if (TYPE_CHOICE == ltype)
	{
// xxx what if right is choice too?
		LinkPtr lch(LinkCast(left));
		for (const Handle& lh : lch->getOutgoingSet())
		{
			if (type_match_rec(lh, right, false)) return true;
		}
		return false;
	}

	// At this point we expect both left an right to be links
	// that are not type-links, i.e. should be ordinary links,
	// e.g. ListLink or EvaluationLink.  Compare these side-by-side.
	if (ltype != rtype) return false;

	LinkPtr lptr(LinkCast(left));
	LinkPtr rptr(LinkCast(right));

	OC_ASSERT(nullptr != lptr and nullptr != rptr, "expecting links!");

	// Unordered links are a pain in the butt.
	if (classserver().isA(ltype, UNORDERED_LINK))
		throw RuntimeException(TRACE_INFO,
			"Not implemented! TODO XXX FIXME");

	const HandleSeq& lout(lptr->getOutgoingSet());
	const HandleSeq& rout(rptr->getOutgoingSet());

	if (lout.size() != rout.size()) return false;

	for (size_t i=0; i< lout.size(); i++)
	{
		if (not type_match_rec(lout[i], rout[i], false)) return false;
	}

	return true;
}

bool opencog::type_match(const Handle& left_, const Handle& right_)
{
	return type_match_rec(left_, right_, true);
}

Handle opencog::type_compose(const Handle& left, const Handle& right)
{
	return Handle::UNDEFINED;
}



/* ===================== END OF FILE ===================== */
