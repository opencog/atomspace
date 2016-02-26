/*
 * TypedAtomLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  May 2015
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

#include <opencog/atoms/base/ClassServer.h>

#include "TypedAtomLink.h"

using namespace opencog;

void TypedAtomLink::init()
{
	// Must have atom and type specification.
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting atom and type specification, got size %s",
			toString().c_str());

	// Perform some additional checks in the UniqueLink init method
	UniqueLink::init(false);

	// Type-check.
	Type stype = _outgoing[0]->getType();
	if (VARIABLE_NODE == stype or
	    GLOB_NODE == stype)
		throw SyntaxException(TRACE_INFO,
			"You are not allowed to globally type a variable");

	Type dtype = _outgoing[1]->getType();
	if (TYPE_NODE != dtype and
	    DEFINED_TYPE_NODE != dtype and
	    TYPE_CHOICE != dtype and
	    SIGNATURE_LINK != dtype and
	    ARROW_LINK != dtype)
		throw SyntaxException(TRACE_INFO,
			"Expecting type defintion, got %s",
				classserver().getTypeName(dtype).c_str());

}

TypedAtomLink::TypedAtomLink(const HandleSeq& oset,
                             TruthValuePtr tv, AttentionValuePtr av)
	: UniqueLink(TYPED_ATOM_LINK, oset, tv, av)
{
	init();
}

TypedAtomLink::TypedAtomLink(const Handle& name, const Handle& defn,
                             TruthValuePtr tv, AttentionValuePtr av)
	: UniqueLink(TYPED_ATOM_LINK, HandleSeq({name, defn}), tv, av)
{
	init();
}

TypedAtomLink::TypedAtomLink(Link &l)
	: UniqueLink(l)
{
	init();
}

/**
 * Get the type description associated with the alias.
 * This will be the second atom of some TypedAtomLink, where
 * `atom` is the first.
 */
Handle TypedAtomLink::get_type(const Handle& atom)
{
	Handle uniq(get_unique(atom, TYPED_ATOM_LINK, false));
	return uniq->getOutgoingAtom(1);
}

/* ===================== END OF FILE ===================== */
