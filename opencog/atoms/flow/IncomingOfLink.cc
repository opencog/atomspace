/*
 * IncomingOfLink.cc
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

#include "IncomingOfLink.h"

using namespace opencog;

IncomingOfLink::IncomingOfLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, INCOMING_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an IncomingOfLink, got %s", tname.c_str());
	}

	size_t sz = _outgoing.size();

	if (1 != sz and 2 != sz)
		throw InvalidParamException(TRACE_INFO,
			"IncomingOfLink expects one or two args, got %lu", sz);
}

// ---------------------------------------------------------------

/// Return a LinkValue vector.
ValuePtr IncomingOfLink::execute(AtomSpace* as, bool silent)
{
	// If the given Atom is executable, then execute it.
	Handle base(_outgoing[0]);
	if (base->is_executable())
	{
		base = HandleCast(base->execute(as, silent));
		if (nullptr == base) return createLinkValue();
	}
	else
	{
		// consume quotes
		Type bt = base->get_type();
		if (DONT_EXEC_LINK == bt or LOCAL_QUOTE_LINK == bt)
			base = base->getOutgoingAtom(0);
	}

	// Simple case. Get IncomingSet.
	if (1 == _outgoing.size())
		return createLinkValue(base->getIncomingSet());

	// Get incoming set by type.
	Handle tnode(_outgoing[1]);
	if (tnode->is_executable())
		tnode = HandleCast(tnode->execute(as, silent));

	if (not tnode->is_type(TYPE_NODE))
		throw RuntimeException(TRACE_INFO,
			"IncomingOfLink expects a type; got %s",
			tnode->to_string().c_str());

	TypeNodePtr tnp = TypeNodeCast(tnode);
	Type intype = tnp->get_kind();
	HandleSeq iset(base->getIncomingSetByType(intype));
	return createLinkValue(iset);
}

DEFINE_LINK_FACTORY(IncomingOfLink, INCOMING_OF_LINK)

/* ===================== END OF FILE ===================== */
