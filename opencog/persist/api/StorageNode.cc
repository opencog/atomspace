/*
 * opencog/persist/api/StorageNode.cc
 *
 * Copyright (c) 2008-2010 OpenCog Foundation
 * Copyright (c) 2009,2013,2020,2022,2025 Linas Vepstas
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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/signature/TypeNode.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include "StorageNode.h"
#include "DispatchHash.h"

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

HandleSeq StorageNode::getMessages() const
{
	static const HandleSeq msgs = []() {
		HandleSeq m({
			createNode(PREDICATE_NODE, "*-open-*"),
			createNode(PREDICATE_NODE, "*-close-*"),
			createNode(PREDICATE_NODE, "*-load-atomspace-*"),
			createNode(PREDICATE_NODE, "*-store-atomspace-*"),
			createNode(PREDICATE_NODE, "*-load-atoms-of-type-*"),
			createNode(PREDICATE_NODE, "*-store-atom-*"),
			createNode(PREDICATE_NODE, "*-store-value-*"),
			createNode(PREDICATE_NODE, "*-update-value-*"),

			createNode(PREDICATE_NODE, "*-fetch-atom-*"),
			createNode(PREDICATE_NODE, "*-fetch-value-*"),
			createNode(PREDICATE_NODE, "*-fetch-incoming-set-*"),
			createNode(PREDICATE_NODE, "*-fetch-incoming-by-type-*"),
			createNode(PREDICATE_NODE, "*-fetch-query-*"),

			createNode(PREDICATE_NODE, "*-delete-*"),
			createNode(PREDICATE_NODE, "*-delete-recursive-*"),
			createNode(PREDICATE_NODE, "*-barrier-*"),

			createNode(PREDICATE_NODE, "*-store-frames-*"),
			createNode(PREDICATE_NODE, "*-delete-frame-*"),
			createNode(PREDICATE_NODE, "*-erase-*"),

			createNode(PREDICATE_NODE, "*-proxy-open-*"),
			createNode(PREDICATE_NODE, "*-proxy-close-*"),
			createNode(PREDICATE_NODE, "*-set-proxy-*"),

			// Used only in getValue
			createNode(PREDICATE_NODE, "*-load-frames-*"),
			createNode(PREDICATE_NODE, "*-connected?-*"),
			createNode(PREDICATE_NODE, "*-load-frames-*"),
			createNode(PREDICATE_NODE, "*-monitor-*")
		});
		// Mark each message predicate as a message, once at load time.
		for (const Handle& h : m)
			h->markIsMessage();
		return m;
	}();

	// Copy list above into the local AtomSpace. If this is not done,
	// then (cog-execute! (IsMessage (Predicate "*-open-*"))) will fail.
	HandleSeq lms;
	for (const Handle& m : msgs)
		lms.emplace_back(_atom_space->add(m));
	return lms;
}

bool StorageNode::usesMessage(const Handle& key) const
{
	static const std::unordered_set<uint32_t> msgset({
		dispatch_hash("*-open-*"),
		dispatch_hash("*-close-*"),
		dispatch_hash("*-load-atomspace-*"),
		dispatch_hash("*-store-atomspace-*"),
		dispatch_hash("*-load-atoms-of-type-*"),
		dispatch_hash("*-store-atom-*"),
		dispatch_hash("*-store-value-*"),
		dispatch_hash("*-update-value-*"),

		dispatch_hash("*-fetch-atom-*"),
		dispatch_hash("*-fetch-value-*"),
		dispatch_hash("*-fetch-incoming-set-*"),
		dispatch_hash("*-fetch-incoming-by-type-*"),
		dispatch_hash("*-fetch-query-*"),

		dispatch_hash("*-delete-*"),
		dispatch_hash("*-delete-recursive-*"),
		dispatch_hash("*-barrier-*"),

		dispatch_hash("*-store-frames-*"),
		dispatch_hash("*-delete-frame-*"),
		dispatch_hash("*-erase-*"),

		dispatch_hash("*-proxy-open-*"),
		dispatch_hash("*-proxy-close-*"),
		dispatch_hash("*-set-proxy-*")
	});

	// Messages used in getValue only don't need to be published here,
	// because only the setValue ones need the COW, R/O work-around.
	// ("*-load-frames-*")
	// ("*-connected?-*")
	// ("*-load-frames-*")
	// ("*-monitor-*")

	if (PREDICATE_NODE != key->get_type()) return false;

	const std::string& pred = key->get_name();
	uint32_t dhsh = dispatch_hash(pred.c_str());
	if (msgset.find(dhsh) != msgset.end()) return true;
	return false;
}

void StorageNode::setValue(const Handle& key, const ValuePtr& value)
{
	// The value must be stored only if it is not one of the values
	// that causes an action to be taken. Action messages must not be
	// recorded, as otherwise, restore from disk/net will cause the
	// action to be triggered!
	if (PREDICATE_NODE != key->get_type())
	{
		Atom::setValue(key, value);
		return;
	}

	// Create a fast dispatch table by using case-statement
	// branching, instead of string compare.
	static constexpr uint32_t p_open =
		dispatch_hash("*-open-*");
	static constexpr uint32_t p_close =
		dispatch_hash("*-close-*");

	static constexpr uint32_t p_load_atomspace =
		dispatch_hash("*-load-atomspace-*");
	static constexpr uint32_t p_store_atomspace =
		dispatch_hash("*-store-atomspace-*");
	static constexpr uint32_t p_load_atoms_of_type =
		dispatch_hash("*-load-atoms-of-type-*");
	static constexpr uint32_t p_store_atom =
		dispatch_hash("*-store-atom-*");
	static constexpr uint32_t p_store_value =
		dispatch_hash("*-store-value-*");
	static constexpr uint32_t p_update_value =
		dispatch_hash("*-update-value-*");

	static constexpr uint32_t p_fetch_atom =
		dispatch_hash("*-fetch-atom-*");
	static constexpr uint32_t p_fetch_value =
		dispatch_hash("*-fetch-value-*");
	static constexpr uint32_t p_fetch_incoming_set =
		dispatch_hash("*-fetch-incoming-set-*");
	static constexpr uint32_t p_fetch_incoming_by_type =
		dispatch_hash("*-fetch-incoming-by-type-*");
	static constexpr uint32_t p_fetch_query =
		dispatch_hash("*-fetch-query-*");

	static constexpr uint32_t p_delete = dispatch_hash("*-delete-*");
	static constexpr uint32_t p_delete_recursive =
		dispatch_hash("*-delete-recursive-*");
	static constexpr uint32_t p_barrier = dispatch_hash("*-barrier-*");

	// *-load-frames-* is in getValue
	static constexpr uint32_t p_store_frames = dispatch_hash("*-store-frames-*");
	static constexpr uint32_t p_delete_frame = dispatch_hash("*-delete-frame-*");
	static constexpr uint32_t p_erase = dispatch_hash("*-erase-*");

	static constexpr uint32_t p_proxy_open = dispatch_hash("*-proxy-open-*");
	static constexpr uint32_t p_proxy_close = dispatch_hash("*-proxy-close-*");
	static constexpr uint32_t p_set_proxy = dispatch_hash("*-set-proxy-*");

// There's almost no chance at all that any user will use some key
// that is a PredicateNode that has a string name that collides with
// one of the above. That's because there's really no reason to set
// static values on a StorageNode. That I can think of. Still there's
// some chance of a hash collision. In this case, define
// COLLISION_PROOF, and recompile. Sorry in advance for the awful
// debug session you had that caused you to discover this comment!
//
// #define COLLISION_PROOF
#ifdef COLLISION_PROOF
	#define COLL(STR) if (0 != pred.compare(STR)) break;
#else
	#define COLL(STR)
#endif

	const std::string& pred = key->get_name();
	switch (dispatch_hash(pred.c_str()))
	{
		case p_open:
			COLL("*-open-*");
			open();
			return;
		case p_close:
			COLL("*-close-*");
			close();
			return;
		case p_load_atomspace:
			COLL("*-load-atomspace-*");
			load_atomspace(AtomSpaceCast(value).get());
			return;
		case p_store_atomspace:
			COLL("*-store-atomspace-*");
			store_atomspace(AtomSpaceCast(value).get());
			return;
		case p_load_atoms_of_type: {
			COLL("*-load-atoms-of-type-*");
			load_atoms_of_type_msg(value);
			return;
		}
		case p_store_atom: {
			COLL("*-store-atom-*");
			if (value->is_type(ATOM))
			{
				store_atom(HandleCast(value));
				return;
			}
			if (not value->is_type(LINK_VALUE)) return;
			const ValueSeq& vsq(LinkValueCast(value)->value());
			for (const ValuePtr& vp : vsq)
				store_atom(HandleCast(vp));
			return;
		}
		case p_store_value: {
			COLL("*-store-value-*");
			if (not value->is_type(LINK_VALUE)) return;
			const ValueSeq& vsq(LinkValueCast(value)->value());
			size_t nv = vsq.size();
			if (2 > nv) return;
			Handle atm(HandleCast(vsq[0]));
			for (size_t i=1; i<nv; i++)
				store_value(atm, HandleCast(vsq[i]));
			return;
		}
		case p_update_value: {
			COLL("*-update-value-*");
			if (not value->is_type(LINK_VALUE)) return;
			const ValueSeq& vsq(LinkValueCast(value)->value());
			if (3 > vsq.size()) return;
			update_value(HandleCast(vsq[0]), HandleCast(vsq[1]), vsq[2]);
			return;
		}
		case p_fetch_atom: {
			COLL("*-fetch-atom-*");
			if (value->is_type(ATOM))
			{
				fetch_atom(HandleCast(value));
				return;
			}
			if (not value->is_type(LINK_VALUE)) return;
			const ValueSeq& vsq(LinkValueCast(value)->value());
			AtomSpace* as = getAtomSpace();
			for (const ValuePtr& vp : vsq)
			{
				if (vp->is_type (ATOM_SPACE))
				{
					as = AtomSpaceCast(HandleCast(vp)).get();
					continue;
				}
				fetch_atom(HandleCast(vp), as);
			}
			return;
		}
		case p_fetch_value: {
			COLL("*-fetch-value-*");
			if (not value->is_type(LINK_VALUE)) return;
			const ValueSeq& vsq(LinkValueCast(value)->value());
			size_t nv = vsq.size();
			// Format: [AtomSpace, Atom, Key1, Key2, ...] or [Atom, Key1, Key2, ...]
			if (3 <= nv && vsq[0]->is_type(ATOM_SPACE)) {
				AtomSpace* as = AtomSpaceCast(vsq[0]).get();
				Handle atm = HandleCast(vsq[1]);
				for (size_t i=2; i<nv; i++)
					fetch_value(atm, HandleCast(vsq[i]), as);
			} else if (2 <= nv) {
				Handle atm = HandleCast(vsq[0]);
				for (size_t i=1; i<nv; i++)
					fetch_value(atm, HandleCast(vsq[i]));
			}
			return;
		}
		case p_fetch_incoming_set: {
			COLL("*-fetch-incoming-set-*");
			if (value->is_type(ATOM))
			{
				fetch_incoming_set(HandleCast(value), false);
				return;
			}
			if (not value->is_type(LINK_VALUE)) return;
			const ValueSeq& vsq(LinkValueCast(value)->value());
			AtomSpace* as = getAtomSpace();
			for (const ValuePtr& vp : vsq)
			{
				if (vp->is_type (ATOM_SPACE))
				{
					as = AtomSpaceCast(HandleCast(vp)).get();
					continue;
				}
				fetch_incoming_set(HandleCast(vp), false, as);
			}
			return;
		}
		case p_fetch_incoming_by_type: {
			COLL("*-fetch-incoming-by-type-*");
			if (not value->is_type(LINK_VALUE)) return;
			const ValueSeq& vsq(LinkValueCast(value)->value());
			size_t nv = vsq.size();
			// Format: [AtomSpace, Atom1, Type1, Atom2, Type2, ...] or [Atom1, Type1, Atom2, Type2, ...]
			if (3 <= nv && vsq[0]->is_type(ATOM_SPACE)) {
				AtomSpace* as = AtomSpaceCast(vsq[0]).get();
				// Process pairs of (Atom, Type) starting from index 1
				for (size_t i=1; i+1<nv; i+=2) {
					Handle atom = HandleCast(vsq[i]);
					Type t = TypeNodeCast(HandleCast(vsq[i+1]))->get_kind();
					fetch_incoming_by_type(atom, t, as);
				}
			} else if (2 <= nv) {
				// Process pairs of (Atom, Type) starting from index 0
				for (size_t i=0; i+1<nv; i+=2) {
					Handle atom = HandleCast(vsq[i]);
					Type t = TypeNodeCast(HandleCast(vsq[i+1]))->get_kind();
					fetch_incoming_by_type(atom, t);
				}
			}
			return;
		}
		case p_fetch_query:
			COLL("*-fetch-query-*");
			fetch_query_msg(value);
			return;
		case p_delete:
			COLL("*-delete-*");
			remove_msg(value, false);
			return;
		case p_delete_recursive:
			COLL("*-delete-recursive-*");
			remove_msg(value, true);
			return;
		case p_barrier:
			COLL("*-barrier-*");
			barrier(AtomSpaceCast(value).get());
			return;
		case p_store_frames:
			COLL("*-store-frames-*");
			store_frames(HandleCast(value));
			return;
		case p_delete_frame:
			COLL("*-delete-frame-*");
			delete_frame(HandleCast(value));
			return;
		case p_erase:
			COLL("*-erase-*");
			erase();
			return;
		case p_proxy_open:
			COLL("*-proxy-open-*");
			proxy_open();
			return;
		case p_proxy_close:
			COLL("*-proxy-close-*");
			proxy_close();
			return;
		case p_set_proxy:
			COLL("*-set-proxy-*");
			set_proxy(HandleCast(value));
			return;
		default:
			break;
	}

	// Some other predicate. Store it.
	Atom::setValue(key, value);
}

ValuePtr StorageNode::getValue(const Handle& key) const
{
	if (PREDICATE_NODE != key->get_type())
		return Atom::getValue(key);

	const std::string& pred(key->get_name());

	if (0 == pred.compare("*-connected?-*"))
	{
		bool is_con = const_cast<StorageNode*>(this)->connected();
		return createBoolValue(is_con);
	}

	if (0 == pred.compare("*-load-frames-*"))
		return createLinkValue(const_cast<StorageNode*>(this)->load_frames());

	if (0 == pred.compare("*-monitor-*"))
		return createStringValue(const_cast<StorageNode*>(this)->monitor());

	return Atom::getValue(key);
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
	// Removal is done with a two-step process. First, we tell storage
	// about the Atom that is going away. It's still in the AtomSpace at
	// this point, so storage can grab whatever data it needs from the
	// AtomSpace (e.g. grab the IncomingSet). Next, we extract from the
	// AtomSpace, and then finally, we tell storage that we're done.
	//
	// The postRemove call gets the return code from the AtomSpace
	// extract. This is because the AtomSpace extract logic is complex,
	// with success & failure depending on subtle interactions between
	// read-only, framing, hiding and other concerns. It is too hard to
	// ask each kind of storage to try to replicate this logic. The
	// solution used here is minimalist: if the AtomSpace extract worked,
	// let storage know. If the AtomSpace extract worked, storage should
	// finish the remove. Otherwise, it should keep the atom.

	if (not recursive and not h->isIncomingSetEmpty()) return false;

	// Removal of atoms from read-only storage is not allowed. However,
	// it is OK to remove atoms from a read-only AtomSpace, because
	// it is acting as a cache for the database, and removal is used
	// used to free up RAM storage.
	if (_atom_space->get_read_only())
		return as->extract_atom(h, recursive);

	// Warn storage about upcoming extraction; do the extraction, then
	// tell storage how it all worked out.
	preRemoveAtom(as, h, recursive);
	bool exok = as->extract_atom(h, recursive);
	postRemoveAtom(as, h, recursive, exok);

	return exok;
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
	if (nullptr == as) as = getAtomSpace();
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
	if (not query->is_executable() and not query->is_evaluatable())
		throw RuntimeException(TRACE_INFO, "Not executable!");

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

void StorageNode::delete_frame(const Handle& has)
{
	return deleteFrame((AtomSpace*)has.get());
}

// ====================================================================

void StorageNode::proxy_open(void)
{
	throw RuntimeException(TRACE_INFO,
		"This StorageNode does not implement proxying!");
}

void StorageNode::proxy_close(void)
{
	throw RuntimeException(TRACE_INFO,
		"This StorageNode does not implement proxying!");
}

void StorageNode::set_proxy(const Handle&)
{
	throw RuntimeException(TRACE_INFO,
		"This StorageNode does not implement proxying!");
}

std::string StorageNode::monitor(void)
{
	return "This StorageNode does not implement a monitor.\n";
}

// ====================================================================
// Message handlers.

// Message handler for remove_atom. Message may be the single Atom
// to be deleted, or a LinkValue giving a mixture of Atoms to be deleted,
// preceded by the AtomSpaces they are to be removed from.
void StorageNode::remove_msg(const ValuePtr& value, bool recursive)
{
	if (value->is_type(ATOM))
	{
		Handle atm(HandleCast(value));
		remove_atom(atm->getAtomSpace(), atm, recursive);
		return;
	}

	// Assume a LinkValue of some kind.
	// ?? Do nothing? Or throw an error? XXX FIXME.
	if (not value->is_type(LINK_VALUE)) return;

	const LinkValuePtr& lvp(LinkValueCast(value));
	AtomSpacePtr asp;
	for (const ValuePtr& vp: lvp->value())
	{
		if (vp->is_type(ATOM_SPACE))
		{
			asp = AtomSpaceCast(vp);
			continue;
		}
		if (asp)
			remove_atom(asp.get(), HandleCast(vp), recursive);
		else
		{
			Handle atm(HandleCast(vp));
			remove_atom(atm->getAtomSpace(), atm, recursive);
		}
	}
}

// Message handler for load_atom_of_type. Message may be the single
// Type to be loaded, or a LinkValue giving a mixture of types,
// preceded by the AtomSpaces they are to loaded into.
void StorageNode::load_atoms_of_type_msg(const ValuePtr& value)
{
	// Expect either a single Type, or a LinkValue of multiple
	// types giving the AtomSpace and the Atom to delete.
	if (value->is_type(TYPE_NODE))
	{
		Handle h(HandleCast(value));
		Type t = TypeNodeCast(h)->get_kind();
		loadType(h->getAtomSpace(), t);
		return;
	}

	// Assume a LinkValue of some kind.
	// ?? Do nothing? Or throw an error? XXX FIXME.
	if (not value->is_type(LINK_VALUE)) return;

	const LinkValuePtr& lvp(LinkValueCast(value));
	AtomSpacePtr asp;
	for (const ValuePtr& vp: lvp->value())
	{
		if (vp->is_type(ATOM_SPACE))
		{
			asp = AtomSpaceCast(vp);
			continue;
		}

		// Ignore anything we don't understand.
		// Or we could throw an exception here. I dunno.
		if (not value->is_type(TYPE_NODE)) continue;

		if (asp)
		{
			Type t = TypeNodeCast(HandleCast(vp))->get_kind();
			loadType(asp.get(), t);
		}
		else
		{
			Handle h(HandleCast(vp));
			Type t = TypeNodeCast(h)->get_kind();
			loadType(h->getAtomSpace(), t);
		}
	}
}

void StorageNode::fetch_query_msg(const ValuePtr& value)
{
	if (not value->is_type(LINK_VALUE)) return;
	const ValueSeq& vsq(LinkValueCast(value)->value());
	size_t nv = vsq.size();
	// Format:
	// [AtomSpace, Query, Key, Metadata, Fresh] or
	// [AtomSpace, Query, Key] or
	// [Query, Key, Metadata, Fresh] or
	// [Query, Key]
	// This is a blargle-mess but I don't feel like cleaning up today.
	// XXX FIXME this should be cleaned up when serious users arrive.
	if (5 <= nv && vsq[0]->is_type(ATOM_SPACE)) {
		AtomSpace* as = AtomSpaceCast(vsq[0]).get();
		Handle query = HandleCast(vsq[1]);
		Handle key = HandleCast(vsq[2]);
		Handle metadata = HandleCast(vsq[3]);
		bool fresh = vsq[4]->is_type(BOOL_VALUE) && BoolValueCast(vsq[4])->get_bit(0);
		fetch_query(query, key, metadata, fresh, as);
	} else if (3 == nv && vsq[0]->is_type(ATOM_SPACE)) {
		AtomSpace* as = AtomSpaceCast(vsq[0]).get();
		Handle query = HandleCast(vsq[1]);
		Handle key = HandleCast(vsq[2]);
		fetch_query(query, key, Handle::UNDEFINED, false, as);
	} else if (4 == nv) {
		Handle query = HandleCast(vsq[0]);
		Handle key = HandleCast(vsq[1]);
		Handle metadata = HandleCast(vsq[2]);
		bool fresh = vsq[3]->is_type(BOOL_VALUE) && BoolValueCast(vsq[3])->get_bit(0);
		fetch_query(query, key, metadata, fresh);
	} else if (2 == nv) {
		Handle query = HandleCast(vsq[0]);
		Handle key = HandleCast(vsq[1]);
		fetch_query(query, key, Handle::UNDEFINED, false);
	}
}

// ====================================================================
