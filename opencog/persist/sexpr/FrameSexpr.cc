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

/// General comments: At this time, the only kinds of "Frames" are
/// AtomSpaces, and so the code below sometimes explcitly works with
/// AtomSpaces.  However, the Frame concept itself is meant to be
/// slightly more general. The idea is that Frames are kind-of-like
/// Nodes, and kind-of-like Links, in that they have a name and also
/// an outgoing set. Unlike Nodes, the name is mutable. Unlike Atoms,
/// Frames are not globally unique (at this time!?).  The general idea
/// is still experimental: it feels like Frames could be more generally
/// useful, but for right now, they are limited to just AtomSpaces.
///
/// The word "Frame" is meant to invoke the general idea of a "Kripke
/// frame". Its a DAG, a poset (a paritally ordereed set), its commonly
/// "complete", and thus resembles the concept of a frame from pointless
/// topology: i.e. a frames-and-locales style frame. The analogy is
/// imprecise.
///
/// The word "Frame" is also meant to invoke the idea of a C stackframe,
/// in that the contents of a Frame (the AtomSpace contents) is a
/// collection of all valid Atoms, for that frame. Since its a DAG, the
/// contents depends on all of the earlier frames, too.

/* ================================================================== */
// Frame printers. Similar to the Atom printers, except
// that frames can have both a name, and an outgoing set.
// At this time, the only Frames are AtomSpaces.
//
// The s-expression has the form
//    ```(AtomSpace "foo" (AtomSpace "bar") (AtomSpace "baz"))```
// which indicates an atomspace named "foo" that is layers above two
// other atomspaces called "bar" and "baz".

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
//
// An example of an s-expression is
//    ```(AtomSpace "foo" (AtomSpace "bar") (AtomSpace "baz"))```
// which indicates an atomspace named "foo" that is layers above two
// other atomspaces called "bar" and "baz".


/// Find some AtomSpace (frame) that has the indicated name.
/// Both the argument, and the returned values are preseumed to be
/// AtomSpacePtr's cast into Handles.
///
static Handle find_frame(const std::string& name, const Handle& surf)
{
	if (0 == name.compare(surf->get_name())) return surf;

	for (const Handle& subf: surf->getOutgoingSet())
	{
		Handle af = find_frame(name, subf);
		if (af) return af;
	}

	return Handle::UNDEFINED;
}

/// Decode an s-expression that encodes a frame. If `surface` is not
/// null, then search for the result in the surface frame. If it can
/// be found there, then return a pointer to it. If not found, return
/// a null pointer.
///
/// If `surface` is null, then create a DAG of AtomSpaces, matching the
/// structure in the s-expression `sframe`.
///
/// The cache is used to skip parsing of long s-expressions, if the
/// named AtomSpace is found in the cache. This assumes that all
/// AtomSpaces have unique names.
///
Handle Sexpr::decode_frame(const Handle& surface,
                           const std::string& sframe, size_t& pos,
                           std::map<std::string, Handle>& cache)
{
	if (std::string::npos == pos) return Handle::UNDEFINED;

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

	size_t r = sframe.size();
	std::string name = get_node_name(sframe, vos, r, FRAME);

	// If we were given a DAG to search, search it.
	if (surface and 0 < surface->get_arity())
	{
		pos = sframe.find(')', r) + 1;
		// Perform a lookup by name only. If found, then return it.
		return find_frame(name, surface);
	}

	// Are there subframes? Loop over them.
	pos = sframe.find_first_not_of(" \n\t", r);
	size_t left = sframe.find('(', pos);
	r = sframe.find(')', pos);

	if (std::string::npos == r)
		throw SyntaxException(TRACE_INFO, "Missing close paren: %s",
			sframe.substr(pos).c_str());

	if (std::string::npos != left and left < r)
	{
		HandleSeq oset;
		bool setbase = true; // Use the base class at most once.
		while (std::string::npos != left and left < r)
		{
			size_t delta = 0;
			Handle frm = decode_frame(
				setbase?surface:Handle::UNDEFINED,
				sframe.substr(left), delta, cache);
			oset.push_back(frm);
			pos = left+delta;
			r = sframe.find(')', pos);
			left = sframe.find('(', pos);
			setbase = false;
		}
		pos = sframe.find_first_not_of(" \n\t", r);
		if (std::string::npos != pos) pos++;

		// See if we already have this space.
		auto it = cache.find(name);
		if (it != cache.end())
			return it->second;

		AtomSpacePtr asp = createAtomSpace(oset);
		asp->set_name(name);
		Handle hasp(HandleCast(asp));
		cache.insert({name, hasp});
		return hasp;
	}

	// If we are here, there are no subframes below us.
	// Skip past closing paren.
	pos = r + 1;

	// See if we already have this space.
	auto it = cache.find(name);
	if (it != cache.end())
		return it->second;

	// If we were given an atomspace with no children, then assume the
	// intent was for these two to be one and the same. That is, the
	// given AtomSpace is meant to be the base of the DAG.
	AtomSpacePtr asp;
	if (surface and 0 == surface->get_arity())
		asp = AtomSpaceCast(surface);
	else
		asp = createAtomSpace();

	asp->set_name(name);
	Handle hasp(HandleCast(asp));
	cache.insert({name, hasp});
	return hasp;
}

/* ============================= END OF FILE ================= */
