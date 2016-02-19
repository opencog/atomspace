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
#include <opencog/util/RandGen.h>

#include <opencog/truthvalue/TruthValue.h>
#include <opencog/truthvalue/AttentionValue.h>

#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>

#include <opencog/atomspace/FixedIntegerIndex.h>
#include <opencog/atomspace/ImportanceIndex.h>
#include <opencog/atomspace/LinkIndex.h>
#include <opencog/atomspace/NodeIndex.h>
#include <opencog/atomspace/TypeIndex.h>

class AtomTableUTest;

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

typedef std::set<AtomPtr> AtomPtrSet;

typedef boost::signals2::signal<void (const Handle&)> AtomSignal;
typedef boost::signals2::signal<void (const AtomPtr&)> AtomPtrSignal;
typedef boost::signals2::signal<void (const Handle&,
                                      const AttentionValuePtr&,
                                      const AttentionValuePtr&)> AVCHSigl;
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
    static std::recursive_mutex _mtx;

    // Cached count of the number of atoms in the table.
    size_t _size;

    // Cached count of the number of atoms of each type.
    std::vector<size_t> _size_by_type;

    // Holds all atoms in the table.  Provides lookup between numeric
    // handle uuid and the actual atom pointer. To some degree, this info
    // is duplicated in the Node and LinkIndex below; we have this here
    // for convenience.
    //
    // This also plays a critical role for memory management: this is
    // the only index that actually holds the atom shared_ptr, and thus
    // increments the atom use count in a guaranteed fashion.  This is
    // the one true guaranteee that the atom will not be deleted while
    // it is in the atom table.
    std::unordered_map<UUID, Handle> _atom_set;

    //!@{
    //! Index for quick retrieval of certain kinds of atoms.
    TypeIndex typeIndex;
    NodeIndex nodeIndex;
    LinkIndex linkIndex;
    ImportanceIndex importanceIndex;

    async_caller<AtomTable, AtomPtr> _index_queue;
    void put_atom_into_index(AtomPtr&);
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

    /** Signal emitted when the AV changes. */
    AVCHSigl _AVChangedSignal;

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
    /**
     * Override and declare copy constructor and equals operator as
     * private.  This is to prevent large object copying by mistake.
     */
    AtomTable& operator=(const AtomTable&);
    AtomTable(const AtomTable&);

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
    UUID get_uuid(void) const { return _uuid; }
    AtomTable* get_environ(void) const { return _environ; }
    AtomSpace* getAtomSpace(void) const { return _as; }

    /**
     * Return true if the atom is in this atomtable, or if it is
     * in the environment of this atomspace.
     */
    bool in_environ(const AtomPtr&) const;

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
    Handle getHandle(Type, std::string) const;
    Handle getHandle(Type, const HandleSeq&) const;
    Handle getHandle(const AtomPtr&) const;
    Handle getHandle(UUID) const;

    AtomPtr do_factory(Type atom_type, AtomPtr atom);
    AtomPtr factory(Type atom_type, AtomPtr atom);
    AtomPtr clone_factory(Type, AtomPtr);

public:
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
             [&](Handle h)->void {
                  (func)(h);
             });
    }

    /**
     * Returns the set of atoms within the given importance range.
     *
     * @param Importance range lower bound (inclusive).
     * @param Importance range upper bound (inclusive).
     * @return The set of atoms within the given importance range.
     */
    UnorderedHandleSet getHandlesByAV(AttentionValue::sti_t lowerBound,
                              AttentionValue::sti_t upperBound = AttentionValue::MAXSTI) const
    {
        std::lock_guard<std::recursive_mutex> lck(_mtx);
        return importanceIndex.getHandleSet(this, lowerBound, upperBound);
    }

    /**
     * Updates the importance index for the given atom. According to the
     * new importance of the atom, it may change importance bins.
     *
     * @param The atom whose importance index will be updated.
     * @param The old importance bin where the atom originally was.
     */
    void updateImportanceIndex(AtomPtr a, int bin)
    {
        if (a->_atomTable != this) return;
        std::lock_guard<std::recursive_mutex> lck(_mtx);
        importanceIndex.updateImportance(a.operator->(), bin);
    }

    /**
     * Adds an atom to the table. If the atom already is in the
     * atomtable, then the truth values and attention values of the
     * two are merged (how, exactly? Is this doe corrrectly!?)
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

    /** Get Node object already in the AtomTable.
     *
     * @param h Handle of the node to retrieve.
     * @return pointer to Node object, NULL if no atom within this AtomTable is
     * associated with handle or if the atom is a link.
     */
    inline NodePtr getNode(Handle& h) const {
        h = getHandle(h); // force resolution of uuid into atom pointer.
        return NodeCast(h);
    }

    /** Get Link object already in the AtomTable.
     *
     * @param h Handle of the link to retrieve.
     * @return pointer to Link object, NULL if no atom within this AtomTable is
     * associated with handle or if the atom is a node.
     */
    inline LinkPtr getLink(Handle& h) const {
        h = getHandle(h); // force resolution of uuid into atom pointer.
        return LinkCast(h);
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

    /** Provide ability for others to find out about AV changes */
    AVCHSigl& AVChangedSignal() { return _AVChangedSignal; }

    /** Provide ability for others to find out about TV changes */
    TVCHSigl& TVChangedSignal() { return _TVChangedSignal; }
};

/** @}*/
} //namespace opencog

#endif // _OPENCOG_ATOMTABLE_H
