/*
 * FrameSexpr.cc
 * Encode/decode S-Expressions for Atomese Frames (AtomSpaces).
 *
 * Copyright (c) 2021 Linas Vepstas <linas@linas.org>
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

/// Decode an s-expression that encodes a frame. Search for the result
/// in the surface frame. If it can be found there, then return a
/// pointer to it. If not found, throw an exception.  This allows the
/// caller to figure out what to do... XXX Maybe we should just create
/// it?
AtomSpace* Sexpr::decode_frame(AtomSpace* surface,
                               const std::string& sframe, size_t& pos)
{
	// Skip past whitespace
	pos = sframe.find_first_not_of(" \n\t", pos);

	// Increment pos by one to point just after the open-paren.
	size_t vos = sframe.find_first_of(" \n\t", ++pos);
	if (std::string::npos == vos
	    or sframe.compare(pos, vos-pos, "AtomSpace"))
		throw SyntaxException(TRACE_INFO, "Badly formatted Frame %s",
			sframe.substr(pos).c_str());

	// Get the AtomSpace name.
	if ('"' != sframe[vos])
		throw SyntaxException(TRACE_INFO, "Badly formatted Frame %s",
			sframe.substr(pos).c_str());
printf("duuuude vosv=%s<<\n", sframe.substr(vos).c_str());


	return nullptr;
}

/* ============================= END OF FILE ================= */
