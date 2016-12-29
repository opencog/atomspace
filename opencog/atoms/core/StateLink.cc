/*
 * StateLink.cc
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

#include "StateLink.h"

using namespace opencog;

void StateLink::init()
{
	// Must have name and body
	if (2 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"Expecting name and state, got size %d", _outgoing.size());

	FreeLink::init();
}

StateLink::StateLink(const HandleSeq& oset,
                     TruthValuePtr tv)
	: UniqueLink(STATE_LINK, oset, tv)
{
	init();
}

StateLink::StateLink(const Handle& name, const Handle& defn,
                     TruthValuePtr tv)
	: UniqueLink(STATE_LINK, HandleSeq({name, defn}), tv)
{
	init();
}

StateLink::StateLink(Link &l)
	: UniqueLink(l)
{
	init();
}

/**
 * Get the state associated with the alias.
 * This will be the second atom of some StateLink, where
 * `alias` is the first.
 */
Handle StateLink::get_state(const Handle& alias)
{
	Handle uniq(get_unique(alias, STATE_LINK, true));
	return uniq->getOutgoingAtom(1);
}

/**
 * Get the link associated with the alias.  This will be the StateLink
 * which has `alias` as the first member of the outgoing set.
 */
Handle StateLink::get_link(const Handle& alias)
{
	return get_unique(alias, STATE_LINK, true);
}

/* ===================== END OF FILE ===================== */
