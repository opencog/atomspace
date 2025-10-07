/*
 * MessagesOfLink.cc
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, Inc.
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

#include <opencog/atoms/value/LinkValue.h>

#include "MessagesOfLink.h"

using namespace opencog;

MessagesOfLink::MessagesOfLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, MESSAGES_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a MessagesOfLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// Return a LinkValue containing all messages from all atoms in the outgoing set.
ValuePtr MessagesOfLink::execute(AtomSpace* as, bool silent)
{
	// The most common case will be just one Atom in the oset.
	if (1 == _outgoing.size())
	{
		const Handle& h(_outgoing[0]);

		if (h->is_executable())
		{
			ValuePtr vp(h->execute(as, silent));
			if (vp->is_atom())
				return createLinkValue(HandleCast(vp)->getMessages());
		}
		return createLinkValue(h->getMessages());
	}

	// Collect all messages from all atoms in the outgoing set
	HandleSet all_messages;
	for (Handle h : _outgoing)
	{
		// If the given Atom is executable, then execute it.
		if (h->is_executable())
		{
			ValuePtr vp(h->execute(as, silent));
			if (vp and vp->is_atom())
				h = HandleCast(vp);
		}

		HandleSeq messages = h->getMessages();
		all_messages.insert(messages.begin(), messages.end());
	}

	return createLinkValue(all_messages);
}

DEFINE_LINK_FACTORY(MessagesOfLink, MESSAGES_OF_LINK)

/* ===================== END OF FILE ===================== */
