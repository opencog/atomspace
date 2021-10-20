/*
 * opencog/persist/api/StorageNode.cc
 *
 * Copyright (c) 2008-2010 OpenCog Foundation
 * Copyright (c) 2009,2013,2020 Linas Vepstas
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

#include <string>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/storage/storage_types.h>
#include "StorageNode.h"

using namespace opencog;

// ====================================================================

StorageNode::StorageNode(Type t, std::string uri) :
	Node(t, uri)
{
	if (not nameserver().isA(t, STORAGE_NODE))
		throw RuntimeException(TRACE_INFO, "Bad inheritance!");
}

StorageNode::~StorageNode()
{
}

std::string StorageNode::monitor(void)
{
	return "This StorageNode does not implement a monitor.";
}

// ====================================================================

void StorageNode::barrier(void)
{
	getAtomSpace()->barrier();
}

void StorageNode::store_atom(const Handle& h)
{
	if (_atom_space->get_read_only())
		throw RuntimeException(TRACE_INFO, "Read-only AtomSpace!");

	storeAtom(h);
}

void StorageNode::store_value(const Handle& h, const Handle& key)
{
	if (_atom_space->get_read_only())
		throw RuntimeException(TRACE_INFO, "Read-only AtomSpace!");

	storeValue(h, key);
}

bool StorageNode::remove_atom(Handle h, bool recursive)
{
    // Removal of atoms from read-only databases is not allowed.
    // It is OK to remove atoms from a read-only atomspace, because
    // it is acting as a cache for the database, and removal is used
    // used to free up RAM storage.
    if (not _atom_space->get_read_only())
        removeAtom(h, recursive);
    return getAtomSpace()->extract_atom(h, recursive);
}

Handle StorageNode::fetch_atom(const Handle& h)
{
	if (nullptr == h) return Handle::UNDEFINED;

	// Now, get the latest values from the backing store.
	// The operation here is to CLOBBER the values, NOT to merge them!
	// The goal of an explicit fetch is to explicitly fetch the values,
	// and not to play monkey-shines with them.  If you want something
	// else, then save the old TV, fetch the new TV, and combine them
	// with your favorite algo.
	Handle ah = _atom_space->add_atom(h);
	if (nullptr == ah) return ah; // if read-only, then cannot update.
	getAtom(ah);
	return ah;
}

Handle StorageNode::fetch_value(const Handle& h, const Handle& key)
{
	// Make sure we are working with Atoms in this Atomspace.
	// Not clear if we really have to do this, or if its enough
	// to just assume  that they are. Could save a few CPU cycles,
	// here, by trading efficiency for safety.
	Handle lkey = getAtomSpace()->add_atom(key);
	Handle lh = getAtomSpace()->add_atom(h);
	loadValue(lh, lkey);
	return lh;
}

Handle StorageNode::fetch_incoming_set(const Handle& h, bool recursive)
{
	// Make sure we are working with Atoms in this Atomspace.
	// Not clear if we really have to do this, or if its enough
	// to just assume  that they are. Could save a few CPU cycles,
	// here, by trading efficiency for safety.
	Handle lh = _atom_space->get_atom(h);
	if (nullptr == lh) return lh;

	// Get everything from the backing store.
	doGetIncomingSet(_atom_space, lh);

	if (not recursive) return lh;

	IncomingSet vh(h->getIncomingSet());
	for (const Handle& lp : vh)
		fetch_incoming_set(lp, true);

	return lh;
}

Handle StorageNode::fetch_incoming_by_type(const Handle& h, Type t)
{
	// Make sure we are working with Atoms in this Atomspace.
	// Not clear if we really have to do this, or if its enough
	// to just assume  that they are. Could save a few CPU cycles,
	// here, by trading efficiency for safety.
	Handle lh = _atom_space->get_atom(h);
	if (nullptr == lh) return lh;

	// Get everything from the backing store.
	doGetIncomingByType(getAtomSpace(), lh, t);

	return lh;
}

Handle StorageNode::fetch_query(const Handle& query, const Handle& key,
							const Handle& metadata, bool fresh)
{
	// At this time, we restrict queries to be ... queries.
	Type qt = query->get_type();
	if (not nameserver().isA(qt, JOIN_LINK) and
		not nameserver().isA(qt, PATTERN_LINK))
		throw RuntimeException(TRACE_INFO, "Not a Join or Meet!");

	// Make sure we are working with Atoms in this Atomspace.
	// Not clear if we really have to do this, or if it's enough
	// to just assume  that they are. Could save a few CPU cycles,
	// here, by trading efficiency for safety.
	Handle lkey = getAtomSpace()->add_atom(key);
	Handle lq = getAtomSpace()->add_atom(query);
	Handle lmeta = metadata;
	if (Handle::UNDEFINED != lmeta) lmeta = getAtomSpace()->add_atom(lmeta);

	runQuery(lq, lkey, lmeta, fresh);
	return lq;
}

void StorageNode::load_atomspace(void)
{
	loadAtomSpace(*getAtomSpace());
}

/**
 * Use the backing store to store entire AtomSpace.
 */
void StorageNode::store_atomspace(void)
{
	storeAtomSpace(*getAtomSpace());
}

void StorageNode::fetch_all_atoms_of_type(Type t)
{
	loadType(*getAtomSpace(), t);
}
