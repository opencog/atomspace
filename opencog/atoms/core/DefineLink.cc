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

#include <opencog/atomspace/ClassServer.h>

#include "DefineLink.h"

using namespace opencog;

void DefineLink::init()
{
	// Must have name and body
	if (2 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"Expecting name and definition, got size %d", _outgoing.size());

	// Perform some additional checks in the UniqueLink init method
	UniqueLink::init(false);
}

DefineLink::DefineLink(const HandleSeq& oset,
                       TruthValuePtr tv, AttentionValuePtr av)
	: UniqueLink(DEFINE_LINK, oset, tv, av)
{
	init();
}

DefineLink::DefineLink(const Handle& name, const Handle& defn,
                       TruthValuePtr tv, AttentionValuePtr av)
	: UniqueLink(DEFINE_LINK, HandleSeq({name, defn}), tv, av)
{
	init();
}

DefineLink::DefineLink(Link &l)
	: UniqueLink(l)
{
	init();
}

/**
 * Get the defintion associated with the alias.
 * This will be the second atom of some DefineLink, where
 * `alias` is the first.
 */
Handle DefineLink::get_definition(const Handle& alias)
{
	Handle uniq(get_unique(alias, DEFINE_LINK, false));
	LinkPtr luniq(LinkCast(uniq));
	return luniq->getOutgoingAtom(1);
}

/* ===================== END OF FILE ===================== */
