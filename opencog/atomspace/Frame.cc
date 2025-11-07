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

#include "AtomSpace.h"
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

	// We cannot remove ourselves directly, via above.
	// However, we can "help", and at least remove other
	// dead weak pointers. This seems ... helpful ...
	for (Handle& h : _outgoing)
		FrameCast(h)->scrub_incoming_set();
}

void Frame::setAtomSpace(AtomSpace* as)
{
	// Under a lock, because ThreadedUTest races in the
	// creation of scratch spaces, which causes issues,'
	// if not protected.
	std::unique_lock<std::shared_mutex> lck(_MTX);
	if (nullptr == _atom_space)
		_atom_space = as;

	else if (nullptr == as)
		_atom_space = nullptr;

	else if (as != _atom_space)
		throw RuntimeException(TRACE_INFO,
			"AtomSpace is already set!\n");
}

/// Place `this` into the incoming set of each outgoing frame.
void Frame::install()
{
	OC_ASSERT(is_type(ATOM_SPACE),
		"Can't deal with anything else right now");

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
	if (not (_flags.load() & USE_ISET_FLAG)) return;
#if USE_BARE_BACKPOINTER
	// This won't work with bare pointers. Which means we have a
	// problem with the validity of the incoming set for Frames:
	// it will include Frames that have been deleted, and thus
	// pointing at freed memory. Oddly enough, no unit test seems
	// to trip on this. But .. well, I guess it's uhh.. maybe bad
	// luck, eh?
	#warning "Using AtomSpace frames with bare pointers is asking for trouble!"
#else
	INCOMING_UNIQUE_LOCK;
	if (not have_inset_map()) return;

	// Iterate over all frame types
	std::vector<Type> framet;
	nameserver().getChildrenRecursive(FRAME, back_inserter(framet));
	InSetMap& iset = get_inset_map();
	for (Type t : framet)
	{
		auto bucket = iset.find(t);
		if (bucket == iset.end()) continue;
		for (auto bi = bucket->second.begin(); bi != bucket->second.end();)
		{
			if (0 == bi->use_count())
#if HAVE_SPARSEHASH
				// sparsehash erase does not invalidate iterators.
				bucket->second.erase(bi);
			bi++;
#else
				bi = bucket->second.erase(bi);
			else bi++;
#endif
		}
	}
#endif
}
