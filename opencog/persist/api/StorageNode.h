/*
 * opencog/persist/api/StorageNode.h
 *
 * Copyright (C) 2008-2011 OpenCog Foundation
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2015,2020,2022 Linas Vepstas
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

#ifndef _OPENCOG_STORAGE_NODE_H
#define _OPENCOG_STORAGE_NODE_H

#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/api/BackingStore.h>
#include <opencog/persist/storage/storage_types.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class StorageNode : public Node, protected BackingStore
{
protected:
	Handle add_nocheck(AtomSpace* as, const Handle& h) const
		{ return as->add(h); }
	void get_absent_atoms(const AtomSpace* as, HandleSeq& missing) const
		{ as->get_absent_atoms(missing); }
	void get_atoms_in_frame(const AtomSpace* as, HandleSeq& fseq) const
		{ as->get_atoms_in_frame(fseq); }

public:
	StorageNode(Type, std::string);
	virtual ~StorageNode();


	// ----------------------------------------------------------------
	// Operations regarding the connection to the remote URI.
	/**
	 * Open a connection to the indicated URI.
	 */
	virtual void open(void) = 0;

	/**
	 * Close an active connection.
	 */
	virtual void close(void) = 0;

	/**
	 * Return true if the connection to the remote end appears to
	 * be established and functioning.
	 */
	virtual bool connected(void) = 0;

	/**
	 * Initialize storage at the remote end. There must already be
	 * an open connection to the remote end; and the remote end must
	 * be vacant or empty.  For example: for an SQL server, this can
	 * be used to create the database, the tables in the database for
	 * the first time.
	 */
	virtual void create(void) = 0;

	/**
	 * Destroy the storage at the remote end. Empties the remote end of
	 * data, and then undoes whaterver `create()` did. Remote ends might
	 * not honor this request, e.g. if other clients have open
	 * connections.
	 */
	virtual void destroy(void) = 0;

	/**
	 * Erase the entire contents of the remote end. Performs a bulk
	 * deletion of all data.
	 */
	virtual void erase(void) = 0;

	/**
	 * For StorageNodes that support proxying, open the proxy for operation.
	 */
	virtual void proxy_open(void);

	/**
	 * For StorageNodes that support proxying, close the proxy for operation.
	 */
	virtual void proxy_close(void);

	/**
	 * For StorageNodes that support proxying, define what the proxy will be.
	 * The argument must be of the form `(FooProxyNode "blah")` or of the
	 * form `(ProxyParameters (FooProxyNode "blah") (BlahBlah...) ...)`.
	 */
	virtual void set_proxy(const Handle&);

	/**
	 * Return debug diagnostics and/or performance monitoring stats.
	 */
	virtual std::string monitor(void);

	// ----------------------------------------------------------------
	// Operations regarding specific atomspace contents.

	/**
	 * Make sure all atom writes have completed, before returning.
	 * This only has an effect when the atomspace is backed by some
	 * sort of storage, or is sending atoms to some remote location
	 * asynchronously. This simply guarantees that the asynch
	 * operations have completed.
	 * NB: at this time, we don't distinguish barrier and flush.
	 */
	void barrier(AtomSpace* = nullptr);

	/**
	 * Unconditionally fetch an atom from the backingstore.
	 *
	 * If there is no backingstore, then Handle::UNDEFINED is returned.
	 * If the atom is found in the backingstore, then it is placed in
	 * the atomtable before returning.  If the atom is already in the
	 * atomtable, and is also found in the backingstore, then all of
	 * the values on the atom are updated from the database. (Values
	 * not in the database are not touched).
	 *
	 * To avoid a fetch if the atom already is in the atomtable, use
	 * the get_atom() method instead.
	 */
	Handle fetch_atom(const Handle&, AtomSpace* = nullptr);

	/**
	 * Fetch the Value located at `key` on `atom` from the remote
	 * server, and attach it to `key` on this `atom`.  If the remote
	 * server does not have a value at `key`, then the local value
	 * at `key` (if any) will be deleted.
	 *
	 * This method is more granular than `fetch_atom()`, as it
	 * operates only on one particular key.
	 */
	Handle fetch_value(const Handle& atom, const Handle& key,
	                   AtomSpace* = nullptr);

	/**
	 * Use the backing store to load all atoms of the given atom type.
	 */
	void fetch_all_atoms_of_type(Type t, AtomSpace* = nullptr);

	/**
	 * Use the backing store to load entire AtomSpace.
	 */
	void load_atomspace(AtomSpace* = nullptr);

	/**
	 * Use the backing store to store entire AtomSpace.
	 */
	void store_atomspace(AtomSpace* = nullptr);

	/**
	 * Return the DAG of all AtomSpaces in the backing store.
	 * The AtomSpaces themselves will not be populated; use the
	 * `load_atomspace` method above to accomplish that.
	 *
	 * The returned HandleSeq consists of AtomSpacePtr's to the
	 * AtomSpaces at the top of the DAG.
	 */
	HandleSeq load_frames(void);

	/**
	 * Store the DAG of all AtomSpaces to the backing store.
	 * The contents of the AtomSpaces themselves will not be stored;
	 * use the `store_atomspace` method above to accomplish that.
	 *
	 * The argument must be an AtomSpacePtr to the top of the DAG.
	 */
	void store_frames(const Handle&);

	/**
	 * Delete the contents of the indicated AtomSpace. This will remove
	 * only those atoms in this particular frame, and not those of any
	 * frame above or below. Atoms marked Absent will also be removed,
	 * which may cause lower atoms to become visible.
	 *
	 * The argument must be an AtomSpacePtr somewhere in the DAG.
	 */
	void delete_frame(const Handle&);

	/**
	 * Use the backing store to load the entire incoming set of the
	 * atom.
	 *
	 * If the flag is true, then the load is done recursively.
	 * This method queries the backing store to obtain all atoms that
	 * contain this one in their outgoing sets. All of these atoms are
	 * then loaded into this atomtable/atomspace.
	 *
	 * See also `fetch_query` which can be used with a JoinLink to
	 * obtain a custom-tailored incoming set.
	 */
	Handle fetch_incoming_set(const Handle&, bool = false, AtomSpace* = nullptr);

	/**
	 * Use the backing store to load the incoming set of the
	 * atom, but only those atoms of the given type.
	 * The fetch is not recursive; that is, only the immediate,
	 * single-level incoming set is fetched.
	 *
	 * See also `fetch_query` which can be used with a JoinLink to
	 * obtain a custom-tailored incoming set.
	 */
	Handle fetch_incoming_by_type(const Handle&, Type, AtomSpace* = nullptr);

	/**
	 * Run the `query` on the remote server, and place the query
	 * results onto the `key`, both locally, and remotely.
	 * The `query` must be either a JoinLink, MeetLink or QueryLink.
	 *
	 * Because MeetLinks and QueryLinks can be cpu-intensive, not
	 * all backends will honor this request. (JoinLinks will be
	 * honored, in general; they can be thought of as a generalized
	 * incoming set, and are much faster to process.) Backends are
	 * free to return previously-cached results for the search,
	 * rather than running a fresh search. If the flag `fresh` is
	 * set to `true`, then the server may interpret this as a
	 * request to perform a fresh search.  It is not required to
	 * honor this request.
	 *
	 * If the `metadata_key` is provided, then metadata about the
	 * search is returned. This may include a time-stamp indicating
	 * when the search was last performed. If the search was refused,
	 * a value indicating that will be returned.  The metadata is
	 * intended to allow the receiver (i.e the user of this local
	 * AtomSpace) what to do next.
	 *
	 * Note that the remote server may periodically purge search
	 * results to save on storage usage. This is why the search
	 * results are returned and placed in the local space.
	 *
	 * Only the Atoms that were the result of the search are returned.
	 * Any Values hanging off those Atoms are not transferred from the
	 * remote server to the local AtomSpace.
	 */
	Handle fetch_query(const Handle& query, const Handle& key,
	                   const Handle& metadata_key = Handle::UNDEFINED,
	                   bool fresh = false,
	                   AtomSpace* = nullptr);

	/**
	 * Recursively store the Atom to the backing store.
	 * I.e. if the Atom is a Link, then store all of the Atoms
	 * in its outgoing set as well, recursively. Store all Keys
	 * and Values attached to the Atom (but not for any Atoms in
	 * it outgoing set; the store of Values is NOT recursive!)
	 */
	void store_atom(const Handle& h);

	/**
	 * Store the Value located at `key` on `atom` to the remote
	 * server. If the `atom` does not yet exist on the remote
	 * server, it is created there.  This method is more granular
	 * than `store_atom` above, as it works with only one value,
	 * instead of all of them.
	 *
	 * Note that Values can be deleted merely by having a null
	 * value hanging on that key.
	 */
	void store_value(const Handle& atom, const Handle& key);

	/**
	 * Update the Value located at `key` on `atom` at the remote
	 * server, incorporating a delta-change `delta`. This is an
	 * atomic read-modify-write update at the server end. The goal
	 * of this method is to allow multiple clients perform delta
	 * changes to values, without race conditions.
	 *
	 * At this time, the only supported updates are increments of
	 * counts: i.e. incremenets of FloatValues. Perhaps other kinds
	 * of updates might be implemented in the future?
	 *
	 * If the `atom` does not yet exist on the remote server, it is
	 * created there.
	 */
	void update_value(const Handle& atom, const Handle& key,
	                  const ValuePtr& delta);

	/**
	 * Removes an atom from the atomspace, and any attached storage.
	 * The atom remains valid as long as there are Handles that
	 * reference it; it is deleted only when the last reference
	 * goes away.
	 *
	 * @param h The Handle of the atom to be removed.
	 * @param recursive Recursive-removal flag. If the flag is set,
	 *       then this atom, and *everything* that points to it will
	 *       be removed from the atomspace.  This can cause a large
	 *       cascade of removals!  If the flag is not set, then the
	 *       atom will be removed only if its incoming set is empty.
	 *       By default, recursion is disabled.
	 * @return True if the Atom for the given Handle was successfully
	 *         removed. False, otherwise.
	 */
	bool remove_atom(AtomSpace*, Handle h, bool recursive=false);
	bool remove_atom(const AtomSpacePtr& as, Handle h, bool recursive=false)
		{ return remove_atom(as.get(), h, recursive); }
};

NODE_PTR_DECL(StorageNode)

typedef std::vector<StorageNodePtr> StorageNodeSeq;

/** @}*/
} // namespace opencog

#endif // _OPENCOG_STORAGE_NODE_H
