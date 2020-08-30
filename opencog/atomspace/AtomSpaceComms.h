/*
 * opencog/atomspace/AtomSpaceComms.h
 *
 * Copyright (C) 2008-2011 OpenCog Foundation
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2015,2020 Linas Vepstas
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

#ifndef _OPENCOG_ATOMSPACE_COMMS_H
#define _OPENCOG_ATOMSPACE_COMMS_H

#include <opencog/persist/api/BackingStore.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class AtomSpaceComms
{
protected:
    /**
     * Used to fetch atoms from disk.
     */
    BackingStore* _backing_store;

    /**
     * Register a provider of backing storage.
     */
    void registerBackingStore(BackingStore*);
    void unregisterBackingStore(BackingStore*);

public:
    bool isAttachedToBackingStore();

public:
    AtomSpaceComms(void) : _backing_store(nullptr) {}
    ~AtomSpaceComms() {}

    /**
     * Make sure all atom writes have completed, before returning.
     * This only has an effect when the atomspace is backed by some
     * sort of storage, or is sending atoms to some remote location
     * asynchronously. This simply guarantees that the asynch
     * operations have completed.
     * NB: at this time, we don't distinguish barrier and flush.
     */
    void barrier(void);

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
    Handle fetch_atom(const Handle&);

    /**
     * Fetch the Value located at `key` on `atom` from the remote
     * server, and attach it to `key` on this `atom`.  If the remote
     * server does not have a value at `key`, then the local value
     * at `key` (if any) will be deleted.
     *
     * This method is more granular than `fetch_atom()`, as it
     * operates only on one particular key.
     */
    Handle fetch_value(const Handle& atom, const Handle& key);

    /**
     * Use the backing store to load all atoms of the given atom type.
     */
    void fetch_all_atoms_of_type(Type t);

    /**
     * Use the backing store to load entire AtomSpace.
     */
    void load_atomspace(void);

    /**
     * Use the backing store to store entire AtomSpace.
     */
    void store_atomspace(void);

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
    Handle fetch_incoming_set(const Handle&, bool=false);

    /**
     * Use the backing store to load the incoming set of the
     * atom, but only those atoms of the given type.
     * The fetch is not recursive; that is, only the immediate,
     * single-level incoming set is fetched.
     *
     * See also `fetch_query` which can be used with a JoinLink to
     * obtain a custom-tailored incoming set.
     */
    Handle fetch_incoming_by_type(const Handle&, Type);

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
     * Any Values hanging off those Atoms are not transfered from the
     * remote server to the local AtomSpace.
     */
    Handle fetch_query(const Handle& query, const Handle& key,
                       const Handle& metadata_key = Handle::UNDEFINED,
                       bool fresh = false);

    /**
     * Recursively store the atom to the backing store.
     * I.e. if the atom is a link, then store all of the atoms
     * in its outgoing set as well, recursively.
     */
    void store_atom(const Handle& h);

    /**
     * Store the Value located at `key` on `atom` to the remote
     * server. If the `atom` does not yet exist on the remote
     * server, it is created there.  This method is more granular
     * than `store_attom` above, as it works with only one value,
     * instead of all of them.
     *
     * Note that Values can be deleted merely by having a null
     * value hanging on that key.
     */
    void store_value(const Handle& atom, const Handle& key);
};

/** @}*/
} // namespace opencog

#endif // _OPENCOG_ATOMSPACE_COMMS_H
