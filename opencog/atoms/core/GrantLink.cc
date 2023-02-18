/*
 * GrantLink.cc
 *
 * Copyright (C) 2015,2023 Linas Vepstas
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

#include "GrantLink.h"

using namespace opencog;

void GrantLink::init(void)
{
	if (not nameserver().isA(get_type(), GRANT_LINK))
		throw SyntaxException(TRACE_INFO,
			"Expecting a GrantLink, got %s",
				nameserver().getTypeName(get_type()).c_str());

	// Must have name and body
	if (2 != _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting name and definition, got size %d", _outgoing.size());

	// Perform some additional checks in the UniqueLink init method
	UniqueLink::init();
}

GrantLink::GrantLink(const HandleSeq&& oset, Type t)
	: UniqueLink(std::move(oset), t)
{
	init();
}

GrantLink::GrantLink(const Handle& name, const Handle& defn)
	: UniqueLink(HandleSeq({name, defn}), GRANT_LINK)
{
	init();
}

/**
 * Get the definition associated with the alias.
 * This will be the second atom of some GrantLink, where
 * `alias` is the first.
 */
Handle GrantLink::get_definition(const Handle& alias, const AtomSpace* as)
{
	Handle uniq(get_unique(alias, GRANT_LINK, false, as));
	return uniq->getOutgoingAtom(1);
}

Handle GrantLink::get_link(const Handle& alias, const AtomSpace* as)
{
	return get_unique(alias, GRANT_LINK, false, as);
}

DEFINE_LINK_FACTORY(GrantLink, GRANT_LINK)

/* ===================== END OF FILE ===================== */
