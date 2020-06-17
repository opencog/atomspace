/*
 * opencog/atoms/core/TypedVariableLink.cc
 *
 * Copyright (C) 2020 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  June 2020
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

#include "TypedVariableLink.h"

using namespace opencog;

void TypedVariableLink::init()
{
	// Must have atom and type specification.
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting atom and type specification; got %s",
			to_string().c_str());

	// Type-check. This is ... kind of a pointless restriction,
	// except that pretty much everything else expects variables
	// in this location.
	Type stype = _outgoing[0]->get_type();
	if (VARIABLE_NODE != stype and
	    GLOB_NODE != stype)
		throw SyntaxException(TRACE_INFO,
			"Sorry, we expect type names to be variables!");

	Type dtype = _outgoing[1]->get_type();
	if (not nameserver().isA(dtype, TYPE_NODE) and
	    DEFINED_TYPE_NODE != dtype and
	    TYPE_CHOICE != dtype and
	    TYPE_SET_LINK != dtype and
	    SIGNATURE_LINK != dtype and
	    INTERVAL_LINK != dtype and
	    ARROW_LINK != dtype)
		throw SyntaxException(TRACE_INFO,
			"Expecting type defintion, got %s",
				nameserver().getTypeName(dtype).c_str());
}

TypedVariableLink::TypedVariableLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	init();
}

TypedVariableLink::TypedVariableLink(const Handle& name, const Handle& defn)
	: Link({name, defn}, TYPED_VARIABLE_LINK)
{
	init();
}

DEFINE_LINK_FACTORY(TypedVariableLink, TYPED_VARIABLE_LINK);

/* ===================== END OF FILE ===================== */
