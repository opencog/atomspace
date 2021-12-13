/*
 * opencog/atoms/core/TypedAtomLink.cc
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
			"Expecting atom and type specification; got %s",
			to_string().c_str());

	// Perform some additional checks in the UniqueLink init method
	UniqueLink::init(false);

	// Type-check.
	Type stype = _outgoing[0]->get_type();
	if (VARIABLE_NODE == stype or
	    GLOB_NODE == stype)
		throw SyntaxException(TRACE_INFO,
			"You are not allowed to globally type a variable");

	Type dtype = _outgoing[1]->get_type();
	if (not nameserver().isA(dtype, TYPE_NODE) and
	    DEFINED_TYPE_NODE != dtype and
	    TYPE_CHOICE != dtype and
	    SIGNATURE_LINK != dtype and
	    ARROW_LINK != dtype)
		throw SyntaxException(TRACE_INFO,
			"Expecting type definition, got %s",
				nameserver().getTypeName(dtype).c_str());

}

TypedAtomLink::TypedAtomLink(const HandleSeq&& oset, Type t)
	: UniqueLink(std::move(oset), t)
{
	init();
}

TypedAtomLink::TypedAtomLink(const Handle& name, const Handle& defn)
	: UniqueLink({name, defn}, TYPED_ATOM_LINK)
{
	init();
}

/**
 * Get the type description associated with the alias.
 * This will be the second atom of some TypedAtomLink, where
 * `atom` is the first.
 */
Handle TypedAtomLink::get_type(const Handle& atom, const AtomSpace* as)
{
	Handle uniq(get_unique(atom, TYPED_ATOM_LINK, false, as));
	return uniq->getOutgoingAtom(1);
}

Handle TypedAtomLink::get_link(const Handle& atom, const AtomSpace* as)
{
	return get_unique(atom, TYPED_ATOM_LINK, false, as);
}

DEFINE_LINK_FACTORY(TypedAtomLink, TYPED_ATOM_LINK);

/* ===================== END OF FILE ===================== */
