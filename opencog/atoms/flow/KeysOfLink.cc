/*
 * KeysOfLink.cc
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

#include "KeysOfLink.h"

using namespace opencog;

KeysOfLink::KeysOfLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, KEYS_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a KeysOfLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// Return a LinkValue containing all keys from all atoms in the outgoing set.
ValuePtr KeysOfLink::execute(AtomSpace* as, bool silent)
{
	// Collect all keys from all atoms in the outgoing set
	HandleSet all_keys;

	for (const Handle& h : _outgoing)
	{
		// If the given Atom is executable, then execute it.
		Handle atom(h);
		if (atom->is_executable())
		{
			atom = HandleCast(atom->execute(as, silent));
			if (nullptr == atom) continue;
		}

		// Get the keys for this atom and merge them into the master set
		HandleSet keys = atom->getKeys();
		all_keys.insert(keys.begin(), keys.end());
	}

	// Convert the set to a sequence and return as LinkValue
	HandleSeq key_seq(all_keys.begin(), all_keys.end());
	return createLinkValue(std::move(key_seq));
}

DEFINE_LINK_FACTORY(KeysOfLink, KEYS_OF_LINK)

/* ===================== END OF FILE ===================== */
