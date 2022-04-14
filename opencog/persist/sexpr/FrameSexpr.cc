/*
 * FrameSexpr.cc
 * Encode/decode S-Expressions for Atomese Frames (AtomSpaces).
 *
 * Copyright (c) 2022 Linas Vepstas <linas@linas.org>
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */
#include <iomanip>

#include <opencog/atomspace/AtomSpace.h>

#include "Sexpr.h"

using namespace opencog;

/* ================================================================== */
// Frame printers. Similar to the Atom printers, except
// that frames can have both a name, and an outgoing set.
// At this time, the only Frames are AtomSpaces.

static std::string prt_frame(const AtomSpace* as)
{
	std::stringstream ss;
	ss << std::quoted(as->get_name());

	std::string txt = "(" + nameserver().getTypeName(as->get_type()) + " ";
	txt += ss.str() + " ";
	for (const Handle& ho : as->getOutgoingSet())
		txt += prt_frame((AtomSpace*) ho.get());
	txt += ")";
	return txt;
}

std::string Sexpr::encode_frame(const Handle& h)
{
	return prt_frame((AtomSpace*) h.get());
}

std::string Sexpr::encode_frame(const AtomSpace* as)
{
	return prt_frame(as);
}

/* ================================================================== */
// Frame decoders. Decode what the above does.

/// Find some AtmSpace (frame) that has the indicated name.
//
// XXX TODO FIXME: should also verify that the subspaces match.
// That is, both the name matches, and also the subframes, too.
static AtomSpace* find_frame(const std::string& name, AtomSpace* surf)
{
	if (0 == name.compare(surf->get_name())) return surf;

	for (const Handle& subf: surf->getOutgoingSet())
	{
		AtomSpace* af = find_frame(name, (AtomSpace*) subf.get());
		if (af) return af;
	}

	return nullptr;
}

/// Decode an s-expression that encodes a frame. Search for the result
/// in the surface frame. If it can be found there, then return a
/// pointer to it. If not found, return a null pointer.
//
// XXX FIXME: this performs a lookup by name only. It should probably
// perform a lookup by inheritance, too. Or maybe that could be provided
// as a distinct function. Its a bit murky, just right now, what the
// right thing to to is.
AtomSpace* Sexpr::decode_frame(AtomSpace* surface,
                               const std::string& sframe, size_t& pos)
{
	size_t totlen = sframe.size();

	// Skip past whitespace
	pos = sframe.find_first_not_of(" \n\t", pos);

	// Increment pos by one to point just after the open-paren.
	size_t vos = sframe.find_first_of(" \n\t", ++pos);
	if (std::string::npos == vos
	    or sframe.compare(pos, vos-pos, "AtomSpace"))
		throw SyntaxException(TRACE_INFO, "Badly formatted Frame %s",
			sframe.substr(pos).c_str());

	// Get the AtomSpace name.
	vos = sframe.find_first_not_of(" \n\t", vos);
	if ('"' != sframe[vos])
		throw SyntaxException(TRACE_INFO, "Badly formatted Frame %s",
			sframe.substr(pos).c_str());

	size_t r = totlen;
	std::string name = get_node_name(sframe, vos, r, FRAME);

	// XXX TODO: verify that the named atomspace has the correct
	// subframes in it, too...

	return find_frame(name, surface);
}

/* ============================= END OF FILE ================= */
