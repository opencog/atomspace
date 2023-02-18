/*
 * DefineLink.cc
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

#include "DefineLink.h"

using namespace opencog;

void DefineLink::init(void)
{
	if (not nameserver().isA(get_type(), DEFINE_LINK))
		throw SyntaxException(TRACE_INFO,
			"Expecting a DefineLink, got %s",
				nameserver().getTypeName(get_type()).c_str());

	// Must have name and body
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting name and definition, got size %d", _outgoing.size());

	// Type-check. The execution and FunctionLink's only expand
	// definitions anchored with these types; other definitions won't
	// work during execution.
	Type dtype = _outgoing[0]->get_type();
	if (DEFINED_PROCEDURE_NODE != dtype and
	    DEFINED_SCHEMA_NODE != dtype and
	    DEFINED_PREDICATE_NODE != dtype and
	    DEFINED_TYPE_NODE != dtype)
		throw SyntaxException(TRACE_INFO,
			"Expecting Defined(Procedure/Schema/Predicate/Type)Node, got %s",
				nameserver().getTypeName(dtype).c_str());

	// Perform some additional checks in the UniqueLink init method
	UniqueLink::init();
}

DefineLink::DefineLink(const HandleSeq&& oset, Type t)
	: UniqueLink(std::move(oset), t)
{
	init();
}

DefineLink::DefineLink(const Handle& name, const Handle& defn)
	: UniqueLink(HandleSeq({name, defn}), DEFINE_LINK)
{
	init();
}

/**
 * Get the definition associated with the alias.
 * This will be the second atom of some DefineLink, where
 * `alias` is the first.
 */
Handle DefineLink::get_definition(const Handle& alias, const AtomSpace* as)
{
	Handle uniq(get_unique(alias, DEFINE_LINK, false, as));
	return uniq->getOutgoingAtom(1);
}

Handle DefineLink::get_link(const Handle& alias, const AtomSpace* as)
{
	return get_unique(alias, DEFINE_LINK, false, as);
}

DEFINE_LINK_FACTORY(DefineLink, DEFINE_LINK)

/* ===================== END OF FILE ===================== */
