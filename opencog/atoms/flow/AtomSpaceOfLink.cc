/*
 * AtomSpaceOfLink.cc
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

#include <opencog/atomspace/AtomSpace.h>

#include "AtomSpaceOfLink.h"

using namespace opencog;

AtomSpaceOfLink::AtomSpaceOfLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, ATOM_SPACE_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an AtomSpaceOfLink, got %s", tname.c_str());
	}

	if (1 != _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"AtomSpaceOfLink expects exactly one argument, got %lu",
			_outgoing.size());
}

// ---------------------------------------------------------------

/// Return the AtomSpace that the atom belongs to.
ValuePtr AtomSpaceOfLink::execute(AtomSpace* as, bool silent)
{
	// If the given Atom is executable, then execute it first.
	Handle base(_outgoing[0]);
	if (base->is_executable())
	{
		ValuePtr result = base->execute(as, silent);
		Handle executed = HandleCast(result);
		// If execution returned an Atom, use it; otherwise keep original
		if (nullptr != executed)
			base = executed;
	}

	// Get the AtomSpace that this atom belongs to
	AtomSpace* atom_as = base->getAtomSpace();

	// If the atom is not in any AtomSpace, return undefined
	if (nullptr == atom_as)
		return Handle::UNDEFINED;

	// Return the AtomSpace as a Handle
	// Explicitly call Atom::get_handle() to bypass AtomSpace's overloaded methods
	return atom_as->Atom::get_handle();
}

DEFINE_LINK_FACTORY(AtomSpaceOfLink, ATOM_SPACE_OF_LINK)

/* ===================== END OF FILE ===================== */
