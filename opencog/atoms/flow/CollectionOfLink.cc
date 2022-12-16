/*
 * CollectionOfLink.cc
 *
 * Copyright (C) 2015, 2022 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/value/LinkValue.h>

#include "CollectionOfLink.h"

using namespace opencog;

CollectionOfLink::CollectionOfLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, COLLECTION_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an CollectionOfLink, got %s", tname.c_str());
	}

	size_t sz = _outgoing.size();

	if (1 != sz and 2 != sz)
		throw InvalidParamException(TRACE_INFO,
			"CollectionOfLink expects one or two args, got %lu", sz);
}

// ---------------------------------------------------------------

/// Return a LinkValue vector.
ValuePtr CollectionOfLink::execute(AtomSpace* as, bool silent)
{
	// If the given Atom is executable, then execute it.
	Handle base(_outgoing[0]);
	if (base->is_executable())
	{
		base = HandleCast(base->execute(as, silent));
		if (nullptr == base) return createLinkValue();
	}

	// Simple case. Get CollectionSet.
	if (1 == _outgoing.size())
		return createLinkValue(base->getCollectionSet());

	// Get incoming set by type.
	Handle tnode(_outgoing[1]);
	if (tnode->is_executable())
		tnode = HandleCast(tnode->execute(as, silent));

	if (not tnode->is_type(TYPE_NODE))
		throw RuntimeException(TRACE_INFO,
			"CollectionOfLink expects a type; got %s",
			tnode->to_string().c_str());

	TypeNodePtr tnp = TypeNodeCast(tnode);
	Type intype = tnp->get_kind();
	HandleSeq iset(base->getCollectionSetByType(intype));
	return createLinkValue(iset);
}

DEFINE_LINK_FACTORY(CollectionOfLink, COLLECTION_OF_LINK)

/* ===================== END OF FILE ===================== */
