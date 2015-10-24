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

#include <algorithm>
#include <list>
#include <set>
#include <vector>

#include <opencog/atomspace/AtomTable.h>
#include <opencog/atomspace/AttentionBank.h>
#include <opencog/atomspace/AttentionValue.h>
#include <opencog/atomspace/BackingStore.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomspace/TruthValue.h>
#include <opencog/util/exceptions.h>

namespace opencog
{
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
    friend class SQLPersistSCM;
    friend class ZMQPersistSCM;
    friend class ::AtomTableUTest;

    /**
     * Override and declare copy constructor and equals operator, to
     * prevent the accidental copying of large objects.
     */
    AtomSpace& operator=(const AtomSpace&);
    AtomSpace(const AtomSpace&);

    AtomTable atomTable;
    AttentionBank bank;
    /**
     * Used to fetch atoms from disk.
     */
    BackingStore *backing_store;

    AtomTable& get_atomtable(void) { return atomTable; }
protected:

    /**
     * Register a provider of backing storage.
     */
    void registerBackingStore(BackingStore *);
    void unregisterBackingStore(BackingStore *);

public:
    AtomSpace(AtomSpace* parent = NULL);
    ~AtomSpace();

    /// Get the environment that this atomspace was created in.
    AtomSpace* get_environ() {
        AtomTable* env = atomTable.get_environ();
        if (env) return env->getAtomSpace();
        return nullptr;
    }

    /**
     * Return the number of atoms contained in the space.
     */
    inline int get_size() const { return atomTable.getSize(); }
    inline int get_num_nodes() const { return atomTable.getNumNodes(); }
    inline int get_num_links() const { return atomTable.getNumLinks(); }
    inline int get_num_atoms_of_type(Type type, bool subclass = false) const
        { return atomTable.getNumAtomsOfType(type, subclass); }
    UUID get_uuid(void) { return atomTable.get_uuid(); }

    //! Clear the atomspace, remove all atoms
    void clear();

    /**
     * Add an atom to the Atom Table.  If the atom already exists
     * then new truth value is ignored, and the existing atom is
     * returned.
     */
    Handle add_atom(AtomPtr atom, bool async=false);

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
        atomTable.barrier();
        if (backing_store) backing_store->barrier();
    }

    /**
     * Unconditionally fetch an atom from the backingstore.
     * If there is no backingstore, then Handle::UNDEINFED is returned.
     * If the atom is found in the backingstore, then it is placed in
     * the atomtable before returning.  If the atom is already in the
     * atomtable, and is also found in the backingstore, then the TV's
     * are merged.
     *
     * The fetch is 'unconditional', in that it is fetched, even if it
     * already is in the atomspace.  Also, the ignored-types of the
     * backing store are not used.
     *
     * To avoid a fetch if the atom already is in the atomtable, use the
     * get_atom() method instead.
     */
    Handle fetch_atom(Handle h);
    Handle fetch_atom(UUID);

    /**
     * Get an atom from the AtomTable. If the atom is not there, then
     * return Handle::UNDEFINED.
     */
    Handle get_atom(const Handle& h) { return atomTable.getHandle(h); }
    Handle get_atom(UUID uuid) { return atomTable.getHandle(uuid); }

    /**
     * Load *all* atoms of the given type, but only if they are not
     * already in the AtomTable.
     */
    void fetch_all_atoms_of_type(Type t) {
        if (NULL == backing_store)
            throw RuntimeException(TRACE_INFO, "No backing store");
        backing_store->loadType(atomTable, t);
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
    void store_atom(Handle h);

    /**
     * Purge an atom from the atomspace.  This only removes the atom
     * from the AtomSpace; it may still remain in persistent storage.
     * To also delete from persistant storage, use the removeAtom()
     * method.  The atom remains valid as long as there are Handles
     * or AtomPtr's that reference it; it is deleted only when the
     * last reference goes away.
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
    bool purge_atom(Handle h, bool recursive = false) {
        return 0 < atomTable.extract(h, recursive).size();
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
	Handle get_link(Type t, Handle ha) {
		return get_link(t, HandleSeq({ha}));
	}
	Handle get_link(Type t, Handle ha, Handle hb) {
		return get_link(t, {ha, hb});
	}
	Handle get_link(Type t, Handle ha, Handle hb, Handle hc) {
		return get_link(t, {ha, hb, hc});
	}
	Handle get_link(Type t, Handle ha, Handle hb, Handle hc, Handle hd) {
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
    bool is_valid_handle(Handle h) const {
        return (NULL != h) and (h->getAtomTable() != NULL);
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
     * Example of call to this method, which would return all entries
     * in AtomSpace:
     * @code
     *         std::list<Handle> ret;
     *         atomSpace.getHandlesByType(back_inserter(ret), ATOM, true);
     * @endcode
     */
    void get_handles_by_type(HandleSeq& appendToHandles,
                             Type type,
                             bool subclass = false) const
    {
        // Get the initial size of the handles vector.
        size_t initial_size = appendToHandles.size();

        // Determine the number of atoms we'll be adding.
        size_t size_of_append = atomTable.getNumAtomsOfType(type, subclass);

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
        return atomTable.getHandlesByType(result, type, subclass);
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

    /* ----------------------------------------------------------- */
    /* Attentional Focus stuff */

    /** Retrieve the doubly normalised Short-Term Importance between -1..1
     * for a given Handle. STI above and below threshold normalised separately
     * and linearly.
     *
     * @param h The atom handle to get STI for
     * @param average Should the recent average max/min STI be used, or the
     * exact min/max?
     * @param clip Should the returned value be clipped to -1..1? Outside this
     * range can be returned if average=true
     * @return normalised STI between -1..1
     */
    float get_normalised_STI(Handle h, bool average=true, bool clip=false) const {
        return bank.getNormalisedSTI(h->getAttentionValue(), average, clip);
    }

    /** Retrieve the linearly normalised Short-Term Importance between 0..1
     * for a given Handle.
     *
     * @param h The atom handle to get STI for
     * @param average Should the recent average max/min STI be used, or the
     * exact min/max?
     * @param clip Should the returned value be clipped to 0..1? Outside this
     * range can be returned if average=true
     * @return normalised STI between 0..1
     */
    float get_normalised_zero_to_one_STI(Handle h, bool average=true, bool clip=false) const {
        return bank.getNormalisedZeroToOneSTI(h->getAttentionValue(), average, clip);
    }

    /**
     * Returns the set of atoms within the given importance range.
     *
     * @param Importance range lower bound (inclusive).
     * @param Importance range upper bound (inclusive).
     * @return The set of atoms within the given importance range.
     *
     * @note: This method utilizes the ImportanceIndex
     */
    template <typename OutputIterator> OutputIterator
    get_handles_by_AV(OutputIterator result,
                      AttentionValue::sti_t lowerBound,
                      AttentionValue::sti_t upperBound = AttentionValue::MAXSTI) const
    {
        UnorderedHandleSet hs = atomTable.getHandlesByAV(lowerBound, upperBound);
        return std::copy(hs.begin(), hs.end(), result);
    }

    /**
     * Gets the set of all handles in the Attentional Focus
     *
     * @return The set of all atoms in the Attentional Focus
     * @note: This method utilizes the ImportanceIndex
     */
    template <typename OutputIterator> OutputIterator
    get_handle_set_in_attentional_focus(OutputIterator result) const
    {
        return get_handles_by_AV(result, get_attentional_focus_boundary(),
                                 AttentionValue::AttentionValue::MAXSTI);
    }

    /** Get attentional focus boundary
     * Generally atoms below this threshold shouldn't be accessed unless search
     * methods are unsuccessful on those that are above this value.
     *
     * @return Short Term Importance threshold value
     */
    AttentionValue::sti_t get_attentional_focus_boundary() const {
        return bank.getAttentionalFocusBoundary();
    }

    /** Change the attentional focus boundary.
     * Some situations may benefit from less focussed searches.
     *
     * @param s New threshold
     * @return Short Term Importance threshold value
     */
    AttentionValue::sti_t set_attentional_focus_boundary(
        AttentionValue::sti_t s) {
        return bank.setAttentionalFocusBoundary(s);
    }

    /** Get the maximum STI observed in the AtomSpace.
     * @param average If true, return an exponentially decaying average of
     * maximum STI, otherwise return the actual maximum.
     * @return Maximum STI
     */
    AttentionValue::sti_t get_max_STI(bool average=true) const
    { return bank.getMaxSTI(average); }

    /** Get the minimum STI observed in the AtomSpace.
     *
     * @param average If true, return an exponentially decaying average of
     * minimum STI, otherwise return the actual maximum.
     * @return Minimum STI
     */
    AttentionValue::sti_t get_min_STI(bool average=true) const
    { return bank.getMinSTI(average); }

    /** Update the minimum STI observed in the AtomSpace.
     * Min/max are not updated on setSTI because average is calculate by lobe
     * cycle, although this could potentially also be handled by the cogServer.
     *
     * @warning Should only be used by attention allocation system.
     * @param m New minimum STI
     */
    void update_min_STI(AttentionValue::sti_t m) { bank.updateMinSTI(m); }

    /**
     * Update the maximum STI observed in the AtomSpace. Min/max are not updated
     * on setSTI because average is calculate by lobe cycle, although this could
     * potentially also be handled by the cogServer.
     *
     * @warning Should only be used by attention allocation system.
     * @param m New maximum STI
     */
    void update_max_STI(AttentionValue::sti_t m) { bank.updateMaxSTI(m); }
    void update_STI_funds(AttentionValue::sti_t m) { bank.updateSTIFunds(m); }
    void update_LTI_funds(AttentionValue::lti_t m) { bank.updateLTIFunds(m); }
    long get_STI_funds() const { return bank.getSTIFunds(); }
    long get_LTI_funds() const { return bank.getLTIFunds(); }

    /* ----------------------------------------------------------- */
    // ---- Signals

    boost::signals2::connection addAtomSignal(const AtomSignal::slot_type& function)
    {
        return atomTable.addAtomSignal().connect(function);
    }
    boost::signals2::connection removeAtomSignal(const AtomPtrSignal::slot_type& function)
    {
        return atomTable.removeAtomSignal().connect(function);
    }
    boost::signals2::connection AVChangedSignal(const AVCHSigl::slot_type& function)
    {
        return atomTable.AVChangedSignal().connect(function);
    }
    boost::signals2::connection TVChangedSignal(const TVCHSigl::slot_type& function)
    {
        return atomTable.TVChangedSignal().connect(function);
    }
    boost::signals2::connection AddAFSignal(const AVCHSigl::slot_type& function)
    {
        return bank.AddAFSignal().connect(function);
    }
    boost::signals2::connection RemoveAFSignal(const AVCHSigl::slot_type& function)
    {
        return bank.RemoveAFSignal().connect(function);
    }

    /* ----------------------------------------------------------- */
    /* Deprecated and obsolete code */

    /**
     * DEPRECATED! DO NOT USE IN NEW CODE!
     * If you need this function, just cut and paste the code below into
     * whatever you are doing!
     */
    template <typename OutputIterator> OutputIterator
    get_handles_by_name(OutputIterator result,
                        const std::string& name,
                        Type type = NODE,
                        bool subclass = true)
    {
        if (name.c_str()[0] == 0)
            return get_handles_by_type(result, type, subclass);

        if (false == subclass) {
            Handle h(get_handle(type, name));
            if (h) *(result++) = h;
            return result;
        }

        classserver().foreachRecursive(
            [&](Type t)->void {
                Handle h(get_handle(t, name));
                if (h) *(result++) = h; }, type);

        return result;
    }

    /**
     * DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this function, just copy the one-liner below.
     * XXX ONLY the python bindings use this. XXX kill that code.
     */
    template <typename OutputIterator> OutputIterator
    get_incoming_set_by_type(OutputIterator result,
                             Handle handle,
                             Type type,
                             bool subclass) const
    {
        return handle->getIncomingSetByType(result, type, subclass);
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    HandleSeq get_incoming(Handle h) const {
        HandleSeq hs;
        h->getIncomingSet(back_inserter(hs));
        return hs;
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    bool is_node(Handle h) const { return NodeCast(h) != NULL; }
    bool is_link(Handle h) const { return LinkCast(h) != NULL; }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    std::string atom_as_string(Handle h, bool terse = true) const {
        if (terse) return h->toShortString();
        return h->toString();
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    const std::string& get_name(Handle h) const {
        static std::string noname;
        NodePtr nnn(NodeCast(h));
        if (nnn) return nnn->getName();
        return noname;
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    void set_STI(Handle h, AttentionValue::sti_t stiValue) const {
        h->setSTI(stiValue);
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    void set_LTI(Handle h, AttentionValue::lti_t ltiValue) const {
        h->setLTI(ltiValue);
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    void inc_VLTI(Handle h) const { h->incVLTI(); }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    void dec_VLTI(Handle h) const { h->decVLTI(); }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    AttentionValue::sti_t get_STI(Handle h) const {
        return h->getAttentionValue()->getSTI();
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    AttentionValue::lti_t get_LTI(Handle h) const {
        return h->getAttentionValue()->getLTI();
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    AttentionValue::vlti_t get_VLTI(Handle h) const {
        return h->getAttentionValue()->getVLTI();
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    const ProtomSeq& get_outgoing(Handle h) const {
        static ProtomSeq empty;
        LinkPtr lll(LinkCast(h));
        if (lll) return lll->getOutgoingSet();
        return empty;
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    Handle get_outgoing(Handle h, Arity idx) const {
        LinkPtr lll = LinkCast(h);
        if (lll) return lll->getOutgoingAtom(idx);
        return Handle::UNDEFINED;
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    Arity get_arity(Handle h) const {
        LinkPtr lll(LinkCast(h));
        if (lll) return lll->getArity();
        return 0;
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    bool is_source(Handle source, Handle link) const
    {
        LinkPtr l(LinkCast(link));
        if (l) return l->isSource(source);
        return false;
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    AttentionValuePtr get_AV(Handle h) const {
        return h->getAttentionValue();
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    void set_AV(Handle h, AttentionValuePtr av) const {
        h->setAttentionValue(av);
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    Type get_type(Handle h) const {
        return h->getType();
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    TruthValuePtr get_TV(Handle h) const
    {
        return h->getTruthValue();
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    strength_t get_mean(Handle h) const {
        return h->getTruthValue()->getMean();
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    confidence_t get_confidence(Handle h) const {
        return h->getTruthValue()->getConfidence();
    }

    /** DEPRECATED! Do NOT USE IN NEW CODE!
     * If you need this, just copy the code below into your app! */
    void set_TV(Handle h, TruthValuePtr tv) const {
        h->setTruthValue(tv);
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
