/*
 * opencog/persist/ati/StorageNode.cc
 *
 * Copyright (c) 2008-2010 OpenCog Foundation
 * Copyright (c) 2009, 2013,2020 Linas Vepstas
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
#include "StorageNode.h"

using namespace opencog;

// ====================================================================

StorageNode::StorageNode(AtomSpace* as) :
	_as(as),
	_atom_table(as->get_atomtable()),
	_backing_store(nullptr)
{
}

StorageNode::~StorageNode()
{
}

bool StorageNode::isAttachedToBackingStore()
{
	if (nullptr != _backing_store) return true;
	return false;
}

void StorageNode::registerBackingStore(BackingStore *bs)
{
	if (isAttachedToBackingStore())
		throw RuntimeException(TRACE_INFO,
			"AtomSpace is already connected to a BackingStore.");

	_backing_store = bs;
}

void StorageNode::unregisterBackingStore(BackingStore *bs)
{
	if (not isAttachedToBackingStore())
		throw RuntimeException(TRACE_INFO,
			"AtomSpace is not connected to a BackingStore.");

	if (bs == _backing_store) _backing_store = nullptr;
}

// ====================================================================

void StorageNode::barrier(void)
{
	_atom_table.barrier();
	if (_backing_store) _backing_store->barrier();
}

void StorageNode::store_atom(const Handle& h)
{
	if (nullptr == _backing_store)
		throw RuntimeException(TRACE_INFO, "No backing store");

	if (_as->get_read_only())
		throw RuntimeException(TRACE_INFO, "Read-only AtomSpace!");

	_backing_store->storeAtom(h);
}

void StorageNode::store_value(const Handle& h, const Handle& key)
{
	if (nullptr == _backing_store)
		throw RuntimeException(TRACE_INFO, "No backing store");

	if (_as->get_read_only())
		throw RuntimeException(TRACE_INFO, "Read-only AtomSpace!");

	_backing_store->storeValue(h, key);
}

bool StorageNode::remove_atom(Handle h, bool recursive)
{
    // Removal of atoms from read-only databases is not allowed.
    // It is OK to remove atoms from a read-only atomspace, because
    // it is acting as a cache for the database, and removal is used
    // used to free up RAM storage.
    if (_backing_store and not _as->get_read_only())
        _backing_store->removeAtom(h, recursive);
    return 0 < _atom_table.extract(h, recursive).size();
}

Handle StorageNode::fetch_atom(const Handle& h)
{
	if (nullptr == _backing_store)
		throw RuntimeException(TRACE_INFO, "No backing store");
	if (nullptr == h) return Handle::UNDEFINED;

	// Now, get the latest values from the backing store.
	// The operation here is to CLOBBER the values, NOT to merge them!
	// The goal of an explicit fetch is to explicitly fetch the values,
	// and not to play monkey-shines with them.  If you want something
	// else, then save the old TV, fetch the new TV, and combine them
	// with your favorite algo.
	Handle ah = _as->add_atom(h);
	if (nullptr == ah) return ah; // if read-only, then cannot update.
	_backing_store->getAtom(ah);
	return ah;
}

Handle StorageNode::fetch_value(const Handle& h, const Handle& key)
{
	if (nullptr == _backing_store)
		throw RuntimeException(TRACE_INFO, "No backing store");

	// Make sure we are working with Atoms in this Atomspace.
	// Not clear if we really have to do this, or if its enough
	// to just assume  that they are. Could save a few CPU cycles,
	// here, by trading efficiency for safety.
	Handle lkey = _atom_table.add(key);
	Handle lh = _atom_table.add(h);
	_backing_store->loadValue(lh, lkey);
	return lh;
}

Handle StorageNode::fetch_incoming_set(const Handle& h, bool recursive)
{
	if (nullptr == _backing_store)
		throw RuntimeException(TRACE_INFO, "No backing store");

	// Make sure we are working with Atoms in this Atomspace.
	// Not clear if we really have to do this, or if its enough
	// to just assume  that they are. Could save a few CPU cycles,
	// here, by trading efficiency for safety.
	Handle lh = _as->get_atom(h);
	if (nullptr == lh) return lh;

	// Get everything from the backing store.
	_backing_store->getIncomingSet(_atom_table, lh);

	if (not recursive) return lh;

	IncomingSet vh(h->getIncomingSet());
	for (const Handle& lp : vh)
		fetch_incoming_set(lp, true);

	return lh;
}

Handle StorageNode::fetch_incoming_by_type(const Handle& h, Type t)
{
	if (nullptr == _backing_store)
		throw RuntimeException(TRACE_INFO, "No backing store");

	// Make sure we are working with Atoms in this Atomspace.
	// Not clear if we really have to do this, or if its enough
	// to just assume  that they are. Could save a few CPU cycles,
	// here, by trading efficiency for safety.
	Handle lh = _as->get_atom(h);
	if (nullptr == lh) return lh;

	// Get everything from the backing store.
	_backing_store->getIncomingByType(_atom_table, lh, t);

	return lh;
}

Handle StorageNode::fetch_query(const Handle& query, const Handle& key,
							const Handle& metadata, bool fresh)
{
	if (nullptr == _backing_store)
		throw RuntimeException(TRACE_INFO, "No backing store");

	// At this time, we restrict queries to be ... queries.
	Type qt = query->get_type();
	if (not nameserver().isA(qt, JOIN_LINK) and
		not nameserver().isA(qt, PATTERN_LINK))
		throw RuntimeException(TRACE_INFO, "Not a Join or Meet!");

	// Make sure we are working with Atoms in this Atomspace.
	// Not clear if we really have to do this, or if it's enough
	// to just assume  that they are. Could save a few CPU cycles,
	// here, by trading efficiency for safety.
	Handle lkey = _atom_table.add(key);
	Handle lq = _atom_table.add(query);
	Handle lmeta = metadata;
	if (Handle::UNDEFINED != lmeta) lmeta = _atom_table.add(lmeta);

	_backing_store->runQuery(lq, lkey, lmeta, fresh);
	return lq;
}

void StorageNode::load_atomspace(void)
{
	if (nullptr == _backing_store)
		throw RuntimeException(TRACE_INFO, "No backing store");
	_backing_store->loadAtomSpace(_atom_table);
}

/**
 * Use the backing store to store entire AtomSpace.
 */
void StorageNode::store_atomspace(void)
{
	if (nullptr == _backing_store)
		throw RuntimeException(TRACE_INFO, "No backing store");
	_backing_store->storeAtomSpace(_atom_table);
}

void StorageNode::fetch_all_atoms_of_type(Type t)
{
	if (nullptr == _backing_store)
		throw RuntimeException(TRACE_INFO, "No backing store");
	_backing_store->loadType(_atom_table, t);
}
