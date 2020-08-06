/*
 * opencog/atomspace/BackingStore.h
 *
 * Implements an interface class for client-server communitcations.
 *
 * Copyright (C) 2009, 2013, 2020 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_BACKING_STORE_H
#define _OPENCOG_BACKING_STORE_H

#include <opencog/util/exceptions.h>
#include <opencog/atoms/base/Atom.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * This class provides a simple, generic interface for communicating
 * Atoms and Values between the local and a remote server (often, a
 * storage or "persistance" server. The general model is that the local
 * AtomSpace is not persistant (it's in RAM, it disappears when the
 * process exits), while the remote server may be persistant (if it is
 * backed to a file or other long-term storage.).
 *
 * This class focuses on "on-demand" atom retreival, rather than on
 * bulk-save/restore.
 *
 * Most methods return `void`, so that they can run asynchronously.
 * That is, there is no requirement that these methods have completed
 * when they return. The only requirement is that all methods called
 * before a call to `barrier()` must have completed by the time that
 * `barrier()` returns.
 */
class BackingStore
{
	friend class BackingImplicator;
	friend class BackingSatisfyingSet;
	friend class BackingJoinCallback;
	public:
		virtual ~BackingStore() {}

		/**
		 * Fetch all the Values attached to the indicated Atom,
		 * if any. The Values are automatically installed on the
		 * provided Atom. If the Atom is in some AtomSpace, then
		 * the keys, as well as any (other) Atoms appearing inside
		 * of the Values are placed into that AtomSpace. (That is,
		 * just provide a null AtomSpace if this is not wanted.)
		 *
		 * See also `loadValue()` below, which can fetch just one
		 * single value.
		 *
		 * FYI: This replaces the deprecated getNode() and getLink().
		 * A backwards-compat implementation is provided.
		 */
		virtual void getAtom(const Handle&);

		/**
		 * Fetch the entire incoming set of the indicated Atom,
		 * and put them into the AtomTable. All of the values attached
		 * to each of the Atoms in the incoming set will be fetched
		 * and the local copies will be updated.
		 *
		 * See also `runQuery()` below. A JoinLink can be thought of
		 * as a generalized incoming set which can be tailored to
		 * more precise needs.
		 */
		virtual void getIncomingSet(AtomTable&, const Handle&) = 0;

		/**
		 * Fetch all Atoms of the given Type in the incoming set of
		 * the indicated Atom, and put them into the AtomTable. All of
		 * the values attached to each of the Atoms in the incoming set
		 * will be fetched as well, and the local copies updated.
		 *
		 * See also `runQuery()` below. A JoinLink can be thought of
		 * as a generalized incoming set which can be tailored to
		 * more precise needs.
		 */
		virtual void getIncomingByType(AtomTable&, const Handle&, Type) = 0;

		/**
		 * Recursively store the Atom and anything in it's outgoing set.
		 * If the Atom is already in storage, this will store or update
		 * the associated Values (TrutheValue, etc) in storage.
		 * If the `synchronous` flag is set, this method will not return
		 * until the atom has actually been stored. (Not all backends will
		 * respect this flag.)
		 *
		 * See also: `storeValue()` below.
		 */
		virtual void storeAtom(const Handle&, bool synchronous = false) = 0;

		/**
		 * Remove the indicated Atom from the backing store.
		 * If the recursive flag is set, then incoming set of the Atom
		 * will also be removed.  If the recursive flag is not set, and
		 * the Atom has a non-empty incoming set, then the Atom will not
		 * be removed.
		 */
		virtual void removeAtom(const Handle&, bool recursive) = 0;

		/**
		 * Store the value located at `key` on `atom` to the remote
		 * server. If the `atom` does not yet exist on the remote
		 * server, it is created there.  This method is more granular
		 * than `StoreAtom` above, as it works with only one value,
		 * instead of all of them.
		 *
		 * Note that Values can be deleted merely by having a null
		 * value hanging on that key.
		 */
		virtual void storeValue(const Handle& atom, const Handle& key)
		{
			throw IOException(TRACE_INFO, "Not implemented!");
		}

		/**
		 * Fetch the Value located at `key` on `atom` from the remote
		 * server, and place it on `key` on `atom` in this AtomSpace.
		 *
		 * This method is more granular than getNode/getLink, as it
		 * operates only on one particular key.
		 */
		virtual void loadValue(const Handle& atom, const Handle& key)
		{
			throw IOException(TRACE_INFO, "Not implemented!");
		}

		/**
		 * Run the `query` on the remote server, and place the results
		 * at `key` on the Atom `query`, both locally, and remotely.
		 * The `query` must be either a JoinLink, MeetLink or QueryLink.
		 *
		 * It is intended that remote servers will usually cache the
		 * query results at `key`, and so future requests for the same
		 * query may return the cached value, instead of running a fresh
		 * query. This is not mandatory: remote servers do not have to
		 * cache, nor do they have to perform the search.
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
		 * Any Values hanging off those Atoms are not transferred from
		 * the remote server to the local AtomSpace.
		 *
		 * FYI Design Note: in principle, I suppose that we could have
		 * this method run any atom that has an `execute()` method on
		 * it. At this time, this is not allowed, for somewhat vague
		 * and arbitrary reasons: (1) we do not want to DDOS the remote
		 * server with heavy CPU processing demands (you can use the
		 * CogServer directly, if you want to do that). We also want to
		 * limit the amount of complexity that the server implementation
		 * must provide. For example, there's a slim chance that traditional
		 * SQL and GraphQL servers might be able to support some of the
		 * simpler queries.  If you want full-function hypergraph query,
		 * just use the CogServer directly.
		 */
		virtual void runQuery(const Handle& query, const Handle& key,
		                      const Handle& metadata_key = Handle::UNDEFINED,
		                      bool fresh=false);

		/**
		 * Fetch *all* Atoms of the given type, and place them into the
		 * AtomTable. All of the associated Values are also be fetched,
		 * and clobber any earlier values that might be present on
		 * existing Atoms. Values on the outgoing set of Type are NOT
		 * fetched!
		 */
		virtual void loadType(AtomTable&, Type) = 0;

		/**
		 * Load *all* atoms from the remote server into this (local)
		 * AtomTable. Also load all Values attached to the Atoms.
		 */
		virtual void loadAtomSpace(AtomTable&) = 0;

		/**
		 * Store *all* atoms from this (local) AtomTable to the remote
		 * server. This stores all Values as well.
		 */
		virtual void storeAtomSpace(const AtomTable&) = 0;

		/**
		 * Read-write synchronization barrier.
		 *
		 * All writes will be completed before this routine returns.
		 * This allows the backend to implement asynchronous writes,
		 * while still providing some local control.
		 *
		 * Note that the callbacks above are NOT synchronizing:
		 * the requested Atoms or Values might not show up until
		 * after the callbacks return! (This depends on the
		 * particular backend). The request operations above *will*
		 * be completed by the time that the barrier returns.
		 * Thus, this call may block for long periods of time,
		 * depending on how much data was buffered up, and how slow
		 * the network is.
		 */
		virtual void barrier() = 0;

		/**
		 * Register this backing store with the atomspace.
		 */
		void registerWith(AtomSpace*);

		/**
		 * Unregister this backing store with the atomspace.
		 */
		void unregisterWith(AtomSpace*);

	protected:
		virtual void getIncomingSet(AtomSpace*, const Handle&);
		virtual void getIncomingByType(AtomSpace*, const Handle&, Type);

		/**  Deprecated. Implement getAtom() instead. */
		virtual Handle getLink(Type, const HandleSeq&) {
			throw IOException(TRACE_INFO, "Implementation is buggy!");
		}
		virtual Handle getNode(Type, const char *) {
			throw IOException(TRACE_INFO, "Implementation is buggy!");
		}
};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_BACKING_STORE_H
