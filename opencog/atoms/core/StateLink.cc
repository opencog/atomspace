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

#include <opencog/atomspace/AtomSpace.h>
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

StateLink::StateLink(const HandleSeq& oset, Type t)
	: UniqueLink(oset, t)
{
	init();
}

StateLink::StateLink(const Handle& name, const Handle& defn)
	: UniqueLink(HandleSeq({name, defn}), STATE_LINK)
{
	init();
}

StateLink::StateLink(const Link &l)
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

void StateLink::install()
{
	// If the handlset is closed (no free variables), then
	// only one copy of the atom can exist in the atomspace.
	if (not is_closed())
	{
		Link::install();
		return;
	}

	// Find all existing copies of this particular StateLink
	// (There should be only one).
	// Perform an atomic swap, replacing the old with the new.
	bool swapped = false;
	const Handle& alias = get_alias();
	IncomingSet defs = alias->getIncomingSetByType(STATE_LINK);
	for (const LinkPtr& defl : defs)
	{
		if (defl->getOutgoingAtom(0) != alias) continue;
		if (defl.get() == this) continue;

		StateLinkPtr old_state(StateLinkCast(defl));
		if (not old_state->is_closed()) continue;

		// Make sure we are visible in the atomspace, before the swap.
		AtomSpace *as = old_state->getAtomSpace();
		setAtomSpace(as);

		// Atomic update of the incoming set.
		const LinkPtr& new_state = LinkCast(get_handle());
		alias->swap_atom(old_state, new_state);

		// Install the other atom as well.
		_outgoing[1]->insert_atom(new_state);

		// Remove the old StateLink too. It must be no more.
		as->remove_atom(defl->get_handle(), true);
		swapped = true;
	}

	if (not swapped) Link::install();
}

DEFINE_LINK_FACTORY(StateLink, STATE_LINK);

/* ===================== END OF FILE ===================== */
