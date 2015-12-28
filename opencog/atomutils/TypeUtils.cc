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
bool opencog::value_is_type(Handle deep, const Handle& val)
{
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

/* ===================== END OF FILE ===================== */
