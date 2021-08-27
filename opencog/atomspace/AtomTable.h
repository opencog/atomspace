/*
 * opencog/atomspace/AtomTable.h
 *
 * Copyright (C) 2008-2010 OpenCog Foundation
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2013,2015 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_ATOMTABLE_H
#define _OPENCOG_ATOMTABLE_H

#include <atomic>
#include <iostream>
#include <set>
#include <vector>

#include <opencog/util/async_method_caller.h>
#include <opencog/util/oc_omp.h>
#include <opencog/util/RandGen.h>
#include <opencog/util/sigslot.h>

#include <opencog/atoms/truthvalue/TruthValue.h>

#include <opencog/atoms/atom_types/NameServer.h>

#include <opencog/atomspace/TypeIndex.h>
#include <opencog/atoms/core/StateLink.h>

class AtomSpaceUTest;
class AtomTableUTest;

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

typedef SigSlot<const Handle&> AtomSignal;
typedef SigSlot<const Handle&,
                const TruthValuePtr&,
                const TruthValuePtr&> TVCHSigl;

class AtomSpace;

/**
 * This class provides mechanisms to store atoms and keep indices for
 * efficient lookups. It implements the local storage data structure of
 * OpenCog. It contains methods to add and remove atoms, as well as to
 * retrieve specific sets according to different criteria.
 */
class AtomTable
{
    friend class ::AtomTableUTest;
    friend class ::AtomSpaceUTest;

private:
    // Single, global mutex for locking the indexes.
    // Its recursive because we need to lock twice during atom insertion
    // and removal: we need to keep the indexes stable while we search
    // them during add/remove.
    mutable std::shared_mutex _mtx;

    //! Index of atoms.
    TypeIndex typeIndex;

    /// Parent environment for this table.  Null if top-level.
    /// This allows atomspaces to be nested; atoms in this atomspace
    /// can reference those in the parent environment.
    /// The UUID is used to uniquely identify it, for distributed
    /// operation. Viz, other computers on the network may have a copy
    /// of this atomtable, and so need to have its UUID to sync up.
    AtomTable* _environ;
    std::atomic_int _num_nested;

    // The AtomSpace that is holding us (if any).
    AtomSpace* _as;
    bool _transient;

    UUID _uuid;

    /** Find out about atom type additions in the NameServer. */
    NameServer& _nameserver;
    int addedTypeConnection;
    void typeAdded(Type);

    /** Provided signals */
    AtomSignal _addAtomSignal;
    AtomSignal _removeAtomSignal;

    /** Signal emitted when the TV changes. */
    TVCHSigl _TVChangedSignal;

    /**
     * Drop copy constructor and equals operator to
     * prevent large object copying by mistake.
     */
    AtomTable& operator=(const AtomTable&) = delete;
    AtomTable(const AtomTable&) = delete;

    void clear_all_atoms();
public:

    /**
     * Constructor and destructor for this class.
     *
     * If 'transient' is true, then some non-essential initialization
     * is skipped.  This makes the constructor run faster. This is
     * useful when the AtomTable is being used only for holding
     * temporary, scratch results, e.g. as a result of evaluation
     * or inference.
     */
    AtomTable(AtomTable* parent=NULL, AtomSpace* holder=NULL,
              bool transient=false);
    ~AtomTable();

    void ready_transient(AtomTable* parent, AtomSpace* holder);
    void clear_transient();

    void clear();

    UUID get_uuid(void) const { return _uuid; }
    AtomTable* get_environ(void) const { return _environ; }
    AtomSpace* getAtomSpace(void) const { return _as; }

    /**
     * Return true if the atom is in this atomtable, or if it is
     * in the environment of this atomtable.
     *
     * This is provided in the header file, so that it gets inlined
     * into Atom.cc, where the incoming link is fetched.  This helps
     * avoid what would otherwise be a circular dependency between
     * shared libraries. Yes, this is kind-of hacky, but its the
     * simplest fix for just right now.
     */
    bool in_environ(const Handle& atom) const
    {
        if (nullptr == atom) return false;
        AtomTable* atab = atom->getAtomTable();
        const AtomTable* env = this;
        while (env) {
            if (atab == env) return true;
            env = env->_environ;
        }
        return false;
    }

    /**
     * Return the number of atoms contained in a table.
     */
    size_t getSize() const;
    size_t getNumNodes() const;
    size_t getNumLinks() const;
    size_t getNumAtomsOfType(Type type, bool subclass=true) const;

    /**
     * Returns the exact atom for the given name and type.
     * Note: Type must inherit from NODE. Otherwise, it returns
     * Handle::UNDEFINED.
     *
     * @param The name of the desired atom.
     * @param The type of the desired atom.
     * @return The handle of the desired atom if found.
     */
    Handle getHandle(Type, const std::string&&) const;
    Handle getHandle(Type, const HandleSeq&&) const;
    Handle getHandle(const Handle&) const;
    Handle lookupHandle(const Handle&) const;

    /**
     * Returns the set of atoms of a given type (subclasses optionally).
     *
     * @param The desired type.
     * @param Whether type subclasses should be considered.
     * @return The set of atoms of a given type (subclasses optionally).
     */
    void
    getHandleSetByType(HandleSet& hset,
                       Type type,
                       bool subclass=false,
                       bool parent=true,
                       AtomSpace* cas = nullptr) const
    {
        if (nullptr == cas) cas = _as;
        std::shared_lock<std::shared_mutex> lck(_mtx);
        auto tit = typeIndex.begin(type, subclass);
        auto tend = typeIndex.end();

        // Iterating over tit++ will just iterate over all atoms
        // of type `type`. That's fine, except for STATE_LINK, where
        // we only want the shallowest state (which over-rides any
        // deeper state).
        // XXX Open issue: we should probably do this for DEFINE_LINK
        // and for TYPED_ATOM_LINK, or anything inheriting from
        // UNIQUE_LINK.
        if (STATE_LINK == type) {
            while (tit != tend) {
                hset.insert(
                    StateLink::get_link(StateLinkCast(*tit)->get_alias(), cas));
                tit++;
            }
        } else {
            while (tit != tend) { hset.insert(*tit); tit++; }
        }

        // If an atom is already in the set, it will hide any duplicate
        // atom in the parent.
        if (parent and _environ)
            _environ->getHandleSetByType(hset, type, subclass, parent, cas);
    }

    /**
     * Returns the set of atoms of a given type, but only if they have
     * and empty outgoing set. This holds the AtomTable lock for a
     * longer period of time, but wastes less RAM when getting big sets.
     * As a net result, it might run faster, maybe.
     *
     * @param The desired type.
     * @param Whether type subclasses should be considered.
     * @return The set of atoms of a given type (subclasses optionally).
     */
    void
    getRootSetByType(HandleSet& hset,
                     Type type,
                     bool subclass=false,
                     bool parent=true) const
    {
        std::shared_lock<std::shared_mutex> lck(_mtx);
        auto tit = typeIndex.begin(type, subclass);
        auto tend = typeIndex.end();
        while (tit != tend) {
            if (0 == (*tit)->getIncomingSetSize())
                 hset.insert(*tit);
            tit++;
        }
        // If an atom is already in the set, it will hide any duplicate
        // atom in the parent.
        if (parent and _environ)
            _environ->getRootSetByType(hset, type, subclass, parent);
    }

    /**
     * Returns the set of atoms of a given type (subclasses optionally).
     *
     * @param The desired type.
     * @param Whether type subclasses should be considered.
     * @return The set of atoms of a given type (subclasses optionally).
     */
    template <typename OutputIterator> OutputIterator
    getHandlesByType(OutputIterator result,
                     Type type,
                     bool subclass=false,
                     bool parent=true) const
    {
        // If parent wanted, and parent exists, then we must use the
        // handleset to disambiguate results.  This causes an extra
        // copy of the handles, unfortunately.
        if (parent and _environ) {
           HandleSet hset;
           getHandleSetByType(hset, type, subclass, parent);
           return std::copy(hset.begin(), hset.end(), result);
        }

        // No parent ... avoid the copy above.
        std::shared_lock<std::shared_mutex> lck(_mtx);
        return std::copy(typeIndex.begin(type, subclass),
                         typeIndex.end(), result);
    }

    /** Calls function 'func' on all atoms */
    template <typename Function> void
    foreachHandleByType(Function func,
                        Type type,
                        bool subclass=false,
                        bool parent=true) const
    {
        // If parent wanted, and parent exists, then we must use the
        // handleset to disambiguate results.  This causes an extra
        // copy of the handles, unfortunately.
        if (parent and _environ) {
           HandleSet hset;
           getHandleSetByType(hset, type, subclass, parent);
           std::for_each(hset.begin(), hset.end(),
                [&](const Handle& h)->void {
                     (func)(h);
                });
           return;
        }

        // No parent ... avoid the copy above.
        // This can (will) deadlock if func touches the table.
        std::shared_lock<std::shared_mutex> lck(_mtx);
        std::for_each(typeIndex.begin(type, subclass),
                      typeIndex.end(),
             [&](const Handle& h)->void {
                  (func)(h);
             });
    }

    template <typename Function> void
    foreachParallelByType(Function func,
                        Type type,
                        bool subclass=false,
                        bool parent=true) const
    {
        // If parent wanted, and parent exists, then we must use the
        // handleset to disambiguate results.  This causes an extra
        // copy of the handles, unfortunately.
        if (parent and _environ) {
           HandleSet hset;
           getHandleSetByType(hset, type, subclass, parent);

           // Parallelize, always, no matter what!
           opencog::setting_omp(opencog::num_threads(), 1);

           OMP_ALGO::for_each(hset.begin(), hset.end(),
                [&](const Handle& h)->void {
                     (func)(h);
                });

           // Reset to default.
           opencog::setting_omp(opencog::num_threads());
           return;
        }

        // No parent ... avoid the copy above.
        // This can (will) deadlock if func touches the table.
        std::shared_lock<std::shared_mutex> lck(_mtx);
        // Parallelize, always, no matter what!
        opencog::setting_omp(opencog::num_threads(), 1);

        OMP_ALGO::for_each(typeIndex.begin(type, subclass),
                      typeIndex.end(),
             [&](const Handle& h)->void {
                  (func)(h);
             });

        // Reset to default.
        opencog::setting_omp(opencog::num_threads());
    }

    /**
     * Adds an atom to the table.
     *
     * The `force` flag forces the addition of this atom into the
     * atomtable, even if it is already in a parent atomspace.
     *
     * @param The new atom to be added.
     * @return The handle of the newly added atom.
     */
    Handle add(const Handle&, bool force=false, bool do_lock=true);

    /**
     * Read-write synchronization barrier fence.  When called, this
     * will not return until all the atoms previously added to the
     * atomspace have been fully inserted.
     */
    void barrier(void);

    /**
     * Return true if the atom table holds this handle, else return false.
     */
    bool holds(const Handle& h) const {
        return (nullptr != h) and h->getAtomTable() == this;
    }

    /**
     * Extracts atoms from the table. Table will not contain the
     * extracted atoms anymore.
     *
     * Note that if the recursive flag is set to false, and the atom
     * appears in the incoming set of some other atom, then extraction
     * will fail.  Thus, it is generally recommended that extraction
     * be recursive, unless you can guarentee that the atom is not in
     * someone else's outgoing set.
     *
     * @param handle The atom to be extracted.
     * @param recursive Recursive-removal flag; if set, the links in the
     *        incoming set will also be extracted.
     * @return A set of the extracted atoms.
     */
    HandleSet extract(Handle& handle, bool recursive=true, bool do_lock=true);

    /**
     * Return a random atom in the AtomTable.
     */
    Handle getRandom(RandGen* rng) const;

    AtomSignal& atomAddedSignal() { return _addAtomSignal; }
    AtomSignal& atomRemovedSignal() { return _removeAtomSignal; }

    /** Provide ability for others to find out about TV changes */
    TVCHSigl& TVChangedSignal() { return _TVChangedSignal; }
};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_ATOMTABLE_H
