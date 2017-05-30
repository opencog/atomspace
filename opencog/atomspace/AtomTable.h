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

#include <iostream>
#include <set>
#include <vector>

#include <boost/signals2.hpp>

#include <opencog/util/async_method_caller.h>
#include <opencog/util/oc_omp.h>
#include <opencog/util/RandGen.h>

#include <opencog/truthvalue/TruthValue.h>

#include <opencog/atoms/base/Quotation.h>
#include <opencog/atoms/base/ClassServer.h>

#include <opencog/atomspace/TypeIndex.h>

class AtomTableUTest;

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

typedef std::set<AtomPtr> AtomPtrSet;

// XXX FIXME boost::signals2 is painfully bloated and slow. It accounts
// for 5% or 10% of the total performance of the atomspace (try it -
// comment out the emit-signal functions below, and measure.
// Alternately, launch gdb, get into the signal, and look at the stack.
// boost::signals2 uses eleven stack frames to do its thing. Eleven!
// Really!) Should be enough to use SigSlot in cogutils.  Need to just
// finish this work.
typedef boost::signals2::signal<void (const Handle&)> AtomSignal;
typedef boost::signals2::signal<void (const AtomPtr&)> AtomPtrSignal;
typedef boost::signals2::signal<void (const Handle&,
                                      const TruthValuePtr&,
                                      const TruthValuePtr&)> TVCHSigl;

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

private:

    // Single, global mutex for locking the indexes.
    // Its recursive because we need to lock twice during atom insertion
    // and removal: we need to keep the indexes stable while we search
    // them during add/remove.
    mutable std::recursive_mutex _mtx;

    // Cached count of the number of atoms in the table.
    size_t _size;
    size_t _num_nodes;
    size_t _num_links;

    // Cached count of the number of atoms of each type.
    std::vector<size_t> _size_by_type;

    // Index of all the atoms in the table, addressible by thier hash.
    std::unordered_multimap<ContentHash, Handle> _atom_store;

    //!@{
    //! Index for quick retrieval of certain kinds of atoms.
    TypeIndex typeIndex;

    async_caller<AtomTable, AtomPtr> _index_queue;
    void put_atom_into_index(const AtomPtr&);
    //!@}

    /**
     * signal connection used to find out about atom type additions in the
     * ClassServer
     */
    boost::signals2::connection addedTypeConnection;

    /** Handler of the 'type added' signal from ClassServer */
    void typeAdded(Type);

    /** Provided signals */
    AtomSignal _addAtomSignal;
    AtomPtrSignal _removeAtomSignal;

    /** Signal emitted when the TV changes. */
    TVCHSigl _TVChangedSignal;

    /// Parent environment for this table.  Null if top-level.
    /// This allows atomspaces to be nested; atoms in this atomspace
    /// can reference those in the parent environment.
    /// The UUID is used to uniquely identify it, for distributed
    /// operation. Viz, other computers on the network may have a copy
    /// of this atomtable, and so need to have its UUID to sync up.
    AtomTable* _environ;
    UUID _uuid;

    // The AtomSpace that is holding us (if any). Needed for DeleteLink operation
    AtomSpace* _as;
    bool _transient;

    /**
     * Override and declare copy constructor and equals operator as
     * private.  This is to prevent large object copying by mistake.
     */
    AtomTable& operator=(const AtomTable&);
    AtomTable(const AtomTable&);

    AtomPtr cast_factory(Type atom_type, AtomPtr atom);
    AtomPtr clone_factory(Type atom_type, AtomPtr atom);

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
    AtomTable(AtomTable* parent = NULL, AtomSpace* holder = NULL,
              bool transient = false);
    ~AtomTable();

    void ready_transient(AtomTable* parent, AtomSpace* holder);
    void clear_transient();

    void clear_all_atoms();
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
    bool in_environ(const AtomPtr& atom) const
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
    size_t getNumAtomsOfType(Type type, bool subclass = true) const;

    /**
     * Returns the exact atom for the given name and type.
     * Note: Type must inherit from NODE. Otherwise, it returns
     * Handle::UNDEFINED.
     *
     * @param The name of the desired atom.
     * @param The type of the desired atom.
     * @return The handle of the desired atom if found.
     */
    Handle getHandle(Type, const std::string&) const;
    Handle getNodeHandle(const AtomPtr&) const;
    Handle getHandle(Type, const HandleSeq&) const;
    Handle getLinkHandle(const AtomPtr&, Quotation quotation = Quotation()) const;
    Handle getHandle(const AtomPtr&, Quotation quotation = Quotation()) const;
    Handle getHandle(const Handle& h) const {
        AtomPtr a(h); return getHandle(a);
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
                     bool subclass = false,
                     bool parent = true) const
    {
        std::lock_guard<std::recursive_mutex> lck(_mtx);
        if (parent && _environ)
            _environ->getHandlesByType(result, type, subclass, parent);
        return std::copy(typeIndex.begin(type, subclass),
                         typeIndex.end(),
                         result);
    }

    /** Calls function 'func' on all atoms */
    template <typename Function> void
    foreachHandleByType(Function func,
                        Type type,
                        bool subclass = false,
                        bool parent = true) const
    {
        std::lock_guard<std::recursive_mutex> lck(_mtx);
        if (parent && _environ)
            _environ->foreachHandleByType(func, type, subclass);
        std::for_each(typeIndex.begin(type, subclass),
                      typeIndex.end(),
             [&](const Handle& h)->void {
                  (func)(h);
             });
    }

    template <typename Function> void
    foreachParallelByType(Function func,
                        Type type,
                        bool subclass = false,
                        bool parent = true) const
    {
        std::lock_guard<std::recursive_mutex> lck(_mtx);
        if (parent && _environ)
            _environ->foreachParallelByType(func, type, subclass);

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

    /* Exposes the type iterators so we can do more complicated 
     * looping without having to create a vector to hold the handles.
     *
     * @param The desired type.
     * @param Whether type subclasses should be considered.
     * @return The handle iterator for the given type.
     */
    TypeIndex::iterator beginType(Type type, bool subclass) const
        { return typeIndex.begin(type, subclass); }
    TypeIndex::iterator endType(void) const
        { return typeIndex.end(); }

    /**
     * Adds an atom to the table. If the atom already is in the
     * atomtable, then the truth values and attention values of the
     * two are merged (how, exactly? Is this done corrrectly!?)
     *
     * If the async flag is set, then the atom addition is performed
     * asynchronously; the atom might not be fully added by the time
     * this method returns, although it will get added eventually.
     * Async addition can improve the multi-threaded performance of
     * lots of parallel adds.  The barrier() method can be used to
     * force synchronization.
     *
     * XXX The async code path doesn't really do anything yet, since
     * it also uses the big global lock, at the moment.  This needs
     * fixing, mostly be creating a second mutex for the atom insertion,
     * and also giving each index its own unique mutex, to avoid
     * collisions.  So the API is here, but more work is still needed.
     *
     * @param The new atom to be added.
     * @return The handle of the newly added atom.
     */
    Handle add(AtomPtr, bool async);

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
        return (NULL != h) and h->getAtomTable() == this;
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
    AtomPtrSet extract(Handle& handle, bool recursive = true);

    /**
     * Return a random atom in the AtomTable.
     */
    Handle getRandom(RandGen* rng) const;

    AtomSignal& addAtomSignal() { return _addAtomSignal; }
    AtomPtrSignal& removeAtomSignal() { return _removeAtomSignal; }

    /** Provide ability for others to find out about TV changes */
    TVCHSigl& TVChangedSignal() { return _TVChangedSignal; }
};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_ATOMTABLE_H
