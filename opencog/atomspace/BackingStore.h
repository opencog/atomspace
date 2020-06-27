/*
 * opencog/atomspace/BackingStore.h
 *
 * Implements an interface class for storage providers.
 *
 * Copyright (C) 2009, 2013 Linas Vepstas <linasvepstas@gmail.com>
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

#include <set>

#include <opencog/atoms/base/Atom.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * This class provides a simple, generic interface for dynamically
 * storing/retreiving atoms from disk or other remote location or
 * process. This class focuses on "on-demand" atom retreival,
 * rather than on bulk-save/restore.
 */
class BackingStore
{
	public:
		virtual ~BackingStore() {}

		/**
		 * Return a Link with the indicated type and outset,
		 * if it exists; else return nullptr. The returned atom
		 * will have all values attached to it, that the backing
		 * store knows about.
		 */
		virtual Handle getLink(Type, const HandleSeq&) = 0;

		/**
		 * Return a Node with the indicated type and name, if it
		 * exists; else return nullptr. The returned atom will have
		 * all values attached to it, that the backing store knows
		 * about.
		 */
		virtual Handle getNode(Type, const char *) = 0;

		/**
		 * Fetch the entire incoming set of the indicated Atom,
		 * and put them into the AtomTable. All of the values attached
		 * to each of the Atoms in the incoming set will be fetched
		 * and the local copies will be updated.
		 */
		virtual void getIncomingSet(AtomTable&, const Handle&) = 0;

		/**
		 * Fetch all Atoms of the given Type in the incoming set of
		 * the indicated Atom, and put them into the AtomTable. All of
		 * the values attached to each of the Atoms in the incoming set
		 * will be fetched as well, and the local copies updated.
		 */
		virtual void getIncomingByType(AtomTable&, const Handle&, Type) = 0;

		/**
		 * Recursively store the Atom and anything in it's outgoing set.
		 * If the Atom is already in storage, this will store or update
		 * the associated Values (TrutheValue, etc) in storage.
		 * If the `synchronous` flag is set, this method will not return
		 * until the atom has actually been stored. (Not all backends will
		 * respect this flag.)
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
		 * Fetch *all* Atoms of the given type, and place them into the
		 * AtomTable. If a given Atom does not yet exist locally, then
		 * all of the Values will also be fetched, and copied locally.
		 * If a given Atom DOES exist locally, then NONE of the local
		 * Values are updated! (This avoidsthe need to make complex
		 * decisions about how to merge conflicting Values).
		 */
		virtual void loadType(AtomTable&, Type) = 0;

		/**
		 * Load *all* atoms.
		 */
		virtual void loadAtomSpace(AtomTable&) = 0;

		/**
		 * Store *all* atoms.
		 */
		virtual void storeAtomSpace(const AtomTable&) = 0;

		/**
		 * Read-write synchronization barrier.
		 * All writes will be completed before this routine returns.
		 * This allows the backend to implement asynchronous writes,
		 * while still providing some control to those who need it.
		 * (Mostly the unit tests, at this time.)
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
};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_BACKING_STORE_H
