/*
 * opencog/atomspace/Frame.cc
 *
 * Copyright (c) 2022 Linas Vepstas
 * All Rights Reserved
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

#include <opencog/atoms/atom_types/NameServer.h>

#include "Frame.h"

using namespace opencog;

void Frame::init()
{
	if (not nameserver().isA(_type, FRAME))
		throw InvalidParamException(TRACE_INFO, "Not a Frame!");

	// Set up the incoming set.
	keep_incoming_set();

	// Cannot call shared_from_this() in the ctor, so cannot call
	// install() here.
	// install();
}

Frame::~Frame()
{
	// Cannot call this in the dtor, because cannot call
	// shared_from_this() in the dtor.
	// remove();

	// Because we cannot remove ourselves directly, via above,
	// we can at least remove other dead weak pointerss.
	for (Handle& h : _outgoing)
		FrameCast(h)->scrub_incoming_set();
}

/// Place `this` into the incoming set of each outgoing frame
///
void Frame::install()
{
	Handle llc(get_handle());
	for (Handle& h : _outgoing)
		h->insert_atom(llc);
}

void Frame::remove()
{
	Handle lll(get_handle());
	for (Handle& h : _outgoing)
		h->remove_atom(lll);
}

/// Remove all dead frames in the incoming set.
void Frame::scrub_incoming_set(void)
{
	if (nullptr == _incoming_set) return;
	INCOMING_UNIQUE_LOCK;

	// Iterate over all frame types
	std::vector<Type> framet;
	std::vector<Type>::iterator it = framet.begin();
	nameserver().getChildrenRecursive(FRAME, it);
	for (Type t : framet)
	{
		auto bucket = _incoming_set->_iset.find(t);
		for (auto bi = bucket->second.begin(); bi != bucket->second.end();)
		{
			// if the weak pointer points at nothing, remove it.
			Handle h(bi->lock());
			if (h) bi++;
			else bi = bucket->second.erase(bi);
		}
	}
}
