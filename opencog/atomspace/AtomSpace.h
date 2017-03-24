/*
 * opencog/atomspace/AtomSpace.h
 *
 * Copyright (C) 2008-2011 OpenCog Foundation
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_ATOMSPACE_H
#define _OPENCOG_ATOMSPACE_H

#include <list>

#include <opencog/util/exceptions.h>
#include <opencog/truthvalue/TruthValue.h>

#include <opencog/atomspace/AtomTable.h>
#include <opencog/atomspace/BackingStore.h>
#include <opencog/atomspace/ValuationTable.h>

namespace opencog
{
const bool EMIT_DIAGNOSTICS = true;
const bool DONT_EMIT_DIAGNOSTICS = false;
const bool CHECK_TRUTH_VALUES = true;
const bool DONT_CHECK_TRUTH_VALUES = false;

/** \addtogroup grp_atomspace
 *  @{
 */
/**
 * The AtomSpace class exposes the public API of the OpenCog AtomSpace
 *
 * @code
 *  // Create an atomspace
 *  AtomSpace atomspace;
 * @endcode
 */
class AtomSpace
{
    friend class Atom;               // Needs to call get_atomtable()
    friend class AtomStorage;
    friend class BackingStore;
    friend class SQLAtomStorage;     // Needs to call get_atomtable()
    friend class ZMQPersistSCM;
    friend class ::AtomTableUTest;

    /**
     * Override and declare copy constructor and equals operator, to
     * prevent the accidental copying of large objects.
     */
    AtomSpace& operator=(const AtomSpace&);
    AtomSpace(const AtomSpace&);

    AtomTable _atom_table;
    ValuationTable _value_table;
    /**
     * Used to fetch atoms from disk.
     */
    BackingStore* _backing_store;

    AtomTable& get_atomtable(void) { return _atom_table; }

    bool _transient;
protected:

    /**
     * Register a provider of backing storage.
     */
    void registerBackingStore(BackingStore*);
    void unregisterBackingStore(BackingStore*);

public:
    AtomSpace(AtomSpace* parent = NULL, bool transient = false);
    ~AtomSpace();

    void ready_transient(AtomSpace* parent);
    void clear_transient();

    /// Get the environment that this atomspace was created in.
    AtomSpace* get_environ() const {
        AtomTable* env = _atom_table.get_environ();
        if (env) return env->getAtomSpace();
        return nullptr;
    }

    /**
     * Compare atomspaces for equality. Useful during testing.
     */
    static bool compare_atomspaces(const AtomSpace& first,
                                   const AtomSpace& second,
                                   bool check_truth_values = CHECK_TRUTH_VALUES,
                                   bool emit_diagnostics = DONT_EMIT_DIAGNOSTICS);
    bool operator==(const AtomSpace& other) const;
    bool operator!=(const AtomSpace& other) const;

    /**
     * Return the number of atoms contained in the space.
     */
    inline size_t get_size() const { return _atom_table.getSize(); }
    inline size_t get_num_nodes() const { return _atom_table.getNumNodes(); }
    inline size_t get_num_links() const { return _atom_table.getNumLinks(); }
    inline size_t get_num_atoms_of_type(Type type, bool subclass = false) const
        { return _atom_table.getNumAtomsOfType(type, subclass); }
    inline UUID get_uuid(void) const { return _atom_table.get_uuid(); }

    //! Clear the atomspace, remove all atoms
    void clear()
        { _atom_table.clear(); }

    /**
     * Add an atom to the Atom Table.  If the atom already exists
     * then new truth value is ignored, and the existing atom is
     * returned.
     */
    Handle add_atom(const Handle&, bool async=false);
    Handle add_atom(AtomPtr a, bool async=false)
        { return add_atom(a->getHandle(), async); }

    /**
     * Add a node to the Atom Table.  If the atom already exists
     * then that is returned.
     *
     * \param t     Type of the node
     * \param name  Name of the node
     */
    Handle add_node(Type t, const std::string& name = "",
                    bool async = false);

    /**
     * Add a link to the Atom Table. If the atom already exists, then
     * that is returned.
     *
     * @param t         Type of the link
     * @param outgoing  a const reference to a HandleSeq containing
     *                  the outgoing set of the link
     */
    Handle add_link(Type t, const HandleSeq& outgoing, bool async = false);

    inline Handle add_link(Type t)
    {
        return add_link(t, HandleSeq{});
    }

    inline Handle add_link(Type t, Handle h)
    {
	    return add_link(t, HandleSeq({h}));
    }

    inline Handle add_link(Type t, Handle ha, Handle hb)
    {
	    return add_link(t, {ha, hb});
    }

    inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc)
    {
        return add_link(t, {ha, hb, hc});
    }

    inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc, Handle hd)
    {
        return add_link(t, {ha, hb, hc, hd});
    }

    inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc,
                           Handle hd, Handle he)
    {
	    return add_link(t, {ha, hb, hc, hd, he});
    }

    inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc,
                           Handle hd, Handle he, Handle hf)
    {
	    return add_link(t, {ha, hb, hc, hd, he, hf});
    }

    inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc,
                           Handle hd, Handle he, Handle hf, Handle hg)
    {
	    return add_link(t, {ha, hb, hc, hd, he, hf, hg});
    }

    inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc,
                           Handle hd, Handle he, Handle hf, Handle hg,
                           Handle hh)
    {
	    return add_link(t, {ha, hb, hc, hd, he, hf, hg, hh});
    }

    inline Handle add_link(Type t, Handle ha, Handle hb, Handle hc,
                           Handle hd, Handle he, Handle hf, Handle hg,
                           Handle hh, Handle hi)
    {
	    return add_link(t, {ha, hb, hc, hd, he, hf, hg, hh, hi});
    }

    /**
     * Make sure all atom writes have completed, before returning.
     * This only has an effect when the atomspace is backed by some
     * sort of storage, or is sending atoms to some remote location
     * asynchronously. This simply guarantees that the asynch
     * operations have completed.
     * NB: at this time, we don't distinguish barrier and flush.
     */
    void barrier(void) {
        _atom_table.barrier();
        if (_backing_store) _backing_store->barrier();
    }

    /**
     * Unconditionally fetch an atom from the backingstore.
     *
     * If there is no backingstore, then Handle::UNDEINFED is returned.
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
     * Get an atom from the AtomTable. If the atom is not there, then
     * return Handle::UNDEFINED.
     */
    Handle get_atom(const Handle& h) const { return _atom_table.getHandle(h); }

    /**
     * Load *all* atoms of the given type, but only if they are not
     * already in the AtomTable.
     */
    void fetch_all_atoms_of_type(Type t) {
        if (NULL == _backing_store)
            throw RuntimeException(TRACE_INFO, "No backing store");
        _backing_store->loadType(_atom_table, t);
    }


    /**
     * Use the backing store to load the entire incoming set of the
     * atom.
     * If the flag is true, then the load is done recursively.
     * This method queries the backing store to obtain all atoms that
     * contain this one in their outgoing sets. All of these atoms are
     * then loaded into this atomtable/atomspace.
     */
    Handle fetch_incoming_set(Handle, bool);

    /**
     * Recursively store the atom to the backing store.
     * I.e. if the atom is a link, then store all of the atoms
     * in its outgoing set as well, recursively.
     */
    void store_atom(const Handle& h);

    /**
     * Extract an atom from the atomspace.  This only removes the atom
     * from the (local, in-RAM) AtomSpace (in this process); any copies
     * of the atom in persistent storage orin other address spaces are
     * unaffected.  To also delete from persistant storage, use the
     * remove_atom() method. Of course, the AtomSpace must be connected
     * to storage in order for remove_atom() to reach out that far; if
     * the AtomSpace is not connected to a backend, there is no
     * difference between remove and extract.
     *
     * The atom itself remains valid as long as there are Handles or
     * AtomPtr's that reference it; the RAM associated with the atom is
     * freed only when the last reference goes away.
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
    bool extract_atom(Handle h, bool recursive = false) {
        return 0 < _atom_table.extract(h, recursive).size();
    }

    /**
     * Removes an atom from the atomspace, and any attached storage.
     * The atom remains valid as long as there are Handles or AtomPtr's
     * that reference it; it is deleted only when the last reference
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
    bool remove_atom(Handle h, bool recursive = false);

    /**
     * Get a node from the AtomTable, if it's in there. If its not found
     * in the AtomTable, and there's a backing store, then the atom will
     * be fetched from the backingstore (and added to the AtomTable). If
     * the atom can't be found in either place, Handle::UNDEFINED will be
     * returned.
     *
     * @param t     Type of the node
     * @param str   Name of the node
    */
    Handle get_node(Type t, const std::string& name = "");
    inline Handle get_handle(Type t, const std::string& str) {
        return get_node(t, str);
    }

    /**
     * Get a link from the AtomTable, if it's in there. If its not found
     * in the AtomTable, and there's a backing store, then the atom will
     * be fetched from the backingstore (and added to the AtomTable). If
     * the atom can't be found in either place, Handle::UNDEFINED will be
     * returned.
     *
     * See also the get_atom() method.
     *
     * @param t        Type of the node
     * @param outgoing a reference to a HandleSeq containing
     *        the outgoing set of the link.
    */
    Handle get_link(Type t, const HandleSeq& outgoing);
    inline Handle get_link(Type t, const Handle& ha) {
        return get_link(t, HandleSeq({ha}));
    }
    Handle get_link(Type t, const Handle& ha, const Handle& hb) {
        return get_link(t, {ha, hb});
    }
    Handle get_link(Type t, const Handle& ha, const Handle& hb, const Handle& hc) {
        return get_link(t, {ha, hb, hc});
    }
    Handle get_link(Type t, const Handle& ha, const Handle& hb, const Handle& hc, const Handle& hd) {
        return get_link(t, {ha, hb, hc, hd});
    }
    Handle get_handle(Type t, const HandleSeq& outgoing) {
        return get_link(t, outgoing);
    }
    Handle get_handle(Type t, const Handle& ha) {
	    return get_handle(t, HandleSeq({ha}));
    }
    Handle get_handle(Type t, const Handle& ha, const Handle& hb) {
	    return get_handle(t, HandleSeq({ha, hb}));
    }

    /**
     * Return true if the handle points to an atom that is in some
     * (any) atomspace; else return false.
     */
    bool is_valid_handle(const Handle& h) const {
        return (NULL != h) and (h->getAtomTable() != NULL);
    }

    /**
     * Gets all the atoms of any type and appends them to the HandleSeq.
     *
     * @param appendToHandles the HandleSeq to which to append the handles.
     *
     * Example of call to this method:
     * @code
     *         std::list<Handle> atoms;
     *         atomSpace.get_all_atoms(atoms);
     * @endcode
     */
    void get_all_atoms(HandleSeq& appendToHandles) const
    {
        // Defer to handles by type for ATOMs and subclasses.
        get_handles_by_type(appendToHandles, ATOM, true);
    }

    /**
     * Gets all the nodes of any type and appends them to the HandleSeq.
     *
     * @param appendToHandles the HandleSeq to which to append the handles.
     *
     * Example of call to this method:
     * @code
     *         std::list<Handle> nodes;
     *         atomSpace.get_all_nodes(nodes);
     * @endcode
     */
    void get_all_nodes(HandleSeq& appendToHandles) const
    {
        // Defer to handles by type for NODEs and subclasses.
        get_handles_by_type(appendToHandles, NODE, true);
    }

    /**
     * Gets all the links of any type and appends them to the HandleSeq.
     *
     * @param appendToHandles the HandleSeq to which to append the handles.
     *
     * Example of call to this method:
     * @code
     *         std::list<Handle> links;
     *         atomSpace.get_all_links(links, LINK, true);
     * @endcode
     */
    void get_all_links(HandleSeq& appendToHandles) const
    {
        // Defer to handles by type for LINKs and subclasses.
        get_handles_by_type(appendToHandles, LINK, true);
    }

    /**
     * Gets a set of handles that matches with the given type
     * (subclasses optionally).
     *
     * @param appendToHandles the HandleSeq to which to append the handles.
     * @param type The desired type.
     * @param subclass Whether type subclasses should be considered.
     *
     * @note The matched entries are appended to a container whose
     *        OutputIterator is passed as the first argument.
     *
     * Example of call to this method, which would return all ConceptNodes
     * in the AtomSpace:
     * @code
     *         std::list<Handle> atoms;
     *         atomSpace.getHandlesByType(atoms, CONCEPT_NODE);
     * @endcode
     */
    void get_handles_by_type(HandleSeq& appendToHandles,
                             Type type,
                             bool subclass = false) const
    {
        // Get the initial size of the handles vector.
        size_t initial_size = appendToHandles.size();

        // Determine the number of atoms we'll be adding.
        size_t size_of_append = _atom_table.getNumAtomsOfType(type, subclass);

        // Now reserve size for the addition. This is faster for large
        // append iterations since appends to the list won't require new
        // allocations and copies whenever the allocated size is exceeded.
        appendToHandles.reserve(initial_size + size_of_append);

        // Now defer to the output iterator call, eating the return.
        get_handles_by_type(back_inserter(appendToHandles), type, subclass);
    }

    /**
     * Gets a set of handles that matches with the given type
     * (subclasses optionally).
     *
     * @param result An output iterator.
     * @param type The desired type.
     * @param subclass Whether type subclasses should be considered.
     *
     * @return The set of atoms of a given type (subclasses optionally).
     *
     * @note The matched entries are appended to a container whose
     *        OutputIterator is passed as the first argument.
     *
     * Example of call to this method, which would return all entries
     * in AtomSpace:
     * @code
     *         std::list<Handle> ret;
     *         atomSpace.getHandlesByType(back_inserter(ret), ATOM, true);
     * @endcode
     */
    template <typename OutputIterator> OutputIterator
    get_handles_by_type(OutputIterator result,
                        Type type,
                        bool subclass = false) const
    {
        return _atom_table.getHandlesByType(result, type, subclass);
    }

    /* ----------------------------------------------------------- */
    /* The foreach routines offer an alternative interface
     * to the getHandleSet API.
     */
    /**
     * Invoke the callback on each handle of the given type.
     */
    template<class T>
    inline bool foreach_handle_of_type(Type atype,
                                       bool (T::*cb)(const Handle&), T *data,
                                       bool subclass = false)
    {
        // First we extract, then we loop. This is to avoid holding
        // the lock for too long. (because we don't know how long
        // the callback will take.)
        std::list<Handle> handle_set;
        // The intended signatue is
        // getHandleSet(OutputIterator result, Type type, bool subclass)
        get_handles_by_type(back_inserter(handle_set), atype, subclass);

        // Loop over all handles in the handle set.
        std::list<Handle>::iterator i = handle_set.begin();
        std::list<Handle>::iterator iend = handle_set.end();
        for (; i != iend; ++i) {
            bool rc = (data->*cb)(*i);
            if (rc) return rc;
        }
        return false;
    }

    /**
     * Convert the atomspace into a string
     */
    std::string to_string() const;

    /* ----------------------------------------------------------- */
    // ---- Signals

    boost::signals2::connection addAtomSignal(const AtomSignal::slot_type& function)
    {
        return _atom_table.addAtomSignal().connect(function);
    }
    boost::signals2::connection removeAtomSignal(const AtomPtrSignal::slot_type& function)
    {
        return _atom_table.removeAtomSignal().connect(function);
    }
    boost::signals2::connection TVChangedSignal(const TVCHSigl::slot_type& function)
    {
        return _atom_table.TVChangedSignal().connect(function);
    }
};

/** @}*/
} // namespace opencog

namespace std {

/**
* Output the whole atomspace, actually all orphans as the others are
* output by of their parents (incoming sets).
*/
ostream& operator<<(ostream&, const opencog::AtomSpace&);

} //namespace std

#endif // _OPENCOG_ATOMSPACE_H
