/*
 * opencog/persist/api/StorageNode.cc
 *
 * Copyright (c) 2008-2010 OpenCog Foundation
 * Copyright (c) 2009,2013,2020,2022 Linas Vepstas
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

void StorageNode::barrier(AtomSpace* as)
{
	if (nullptr == as) as = getAtomSpace();
	as->barrier();
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

void StorageNode::update_value(const Handle& h, const Handle& key,
                               const ValuePtr& delta)
{
	if (_atom_space->get_read_only())
		throw RuntimeException(TRACE_INFO, "Read-only AtomSpace!");

	updateValue(h, key, delta);
}

bool StorageNode::remove_atom(AtomSpace* as, Handle h, bool recursive)
{
	// Removal is ... tricky. We need to remove Atoms from storage first,
	// and only then the AtomSpace. We need to do storage first because
	// storage needs access to the incoming set, and other Atom things,
	// which get destroyed by the AtomSpace removal. However, we should
	// remove from storage only if the AtomSpace remove succeeds! The
	// AtomSpace remove logic is complex, and we cannot replicate it
	// here. Thus we have a chicken-and-egg situation. The solution used
	// here is minimalist: do only as much as needed to stay compatible
	// with the docs.
	//
	// Unfortunately, this means the code won't be thread-safe. If one
	// thread is adding atoms while another is deleting them, we will
	// easily and rapidly arrive in a situation where the storage and
	// the AtomSpace are not in sync: they won't have the same Atoms.
	// This is a Caveat-Emptor situation: its up to the user to solve
	// these races.

	if (not recursive and not h->isIncomingSetEmpty()) return false;

	// Removal of atoms from read-only databases is not allowed.
	// It is OK to remove atoms from a read-only AtomSpace, because
	// it is acting as a cache for the database, and removal is used
	// used to free up RAM storage.
	if (not _atom_space->get_read_only())
		removeAtom(as, h, recursive);

	return as->extract_atom(h, recursive);
}

Handle StorageNode::fetch_atom(const Handle& h, AtomSpace* as)
{
	if (nullptr == h) return Handle::UNDEFINED;
	if (nullptr == as) as = getAtomSpace();

	// Now, get the latest values from the backing store.
	// The operation here is to CLOBBER the values, NOT to merge them!
	// The goal of an explicit fetch is to explicitly fetch the values,
	// and not to play monkey-shines with them.  If you want something
	// else, then save the old TV, fetch the new TV, and combine them
	// with your favorite algo.
	Handle ah = as->add_atom(h);
	if (nullptr == ah) return ah; // if read-only, then cannot update.
	getAtom(ah);
	return ah;
}

Handle StorageNode::fetch_value(const Handle& h, const Handle& key,
                                AtomSpace* as)
{
	Handle lkey = as->add_atom(key);
	Handle lh = as->add_atom(h);
	loadValue(lh, lkey);
	return lh;
}

Handle StorageNode::fetch_incoming_set(const Handle& h, bool recursive,
                                       AtomSpace* as)
{
	if (nullptr == as) as = getAtomSpace();
	Handle lh = as->get_atom(h);
	if (nullptr == lh) return lh;

	// Get everything from the backing store.
	fetchIncomingSet(as, lh);

	if (not recursive) return lh;

	IncomingSet vh(h->getIncomingSet());
	for (const Handle& lp : vh)
		fetch_incoming_set(lp, true, as);

	return lh;
}

Handle StorageNode::fetch_incoming_by_type(const Handle& h, Type t,
                                           AtomSpace* as)
{
	if (nullptr == as) as = getAtomSpace();
	Handle lh = as->get_atom(h);
	if (nullptr == lh) return lh;

	// Get everything from the backing store.
	fetchIncomingByType(as, lh, t);

	return lh;
}

Handle StorageNode::fetch_query(const Handle& query, const Handle& key,
                                const Handle& metadata, bool fresh,
                                AtomSpace* as)
{
	// Queries can be anything executable or evaluatable.
	Type qt = query->get_type();
	if (not nameserver().isA(qt, JOIN_LINK) and
		not nameserver().isA(qt, PATTERN_LINK) and
		not nameserver().isA(qt, FUNCTION_LINK) and
		not nameserver().isA(qt, EVALUATABLE_LINK))
		throw RuntimeException(TRACE_INFO, "Not a Join or Meet!");

	if (nullptr == as) as = getAtomSpace();
	Handle lkey = as->add_atom(key);
	Handle lq = as->add_atom(query);
	Handle lmeta = metadata;
	if (Handle::UNDEFINED != lmeta) lmeta = as->add_atom(lmeta);

	runQuery(lq, lkey, lmeta, fresh);
	return lq;
}

void StorageNode::load_atomspace(AtomSpace* as)
{
	if (nullptr == as) as = getAtomSpace();
	loadAtomSpace(as);
}

/**
 * Use the backing store to store entire AtomSpace.
 */
void StorageNode::store_atomspace(AtomSpace* as)
{
	if (nullptr == as) as = getAtomSpace();
	storeAtomSpace(as);
}

void StorageNode::fetch_all_atoms_of_type(Type t, AtomSpace* as)
{
	if (nullptr == as) as = getAtomSpace();
	loadType(as, t);
}

HandleSeq StorageNode::load_frames(void)
{
	return loadFrameDAG();
}

void StorageNode::store_frames(const Handle& has)
{
	return storeFrameDAG((AtomSpace*)has.get());
}

// ====================================================================
