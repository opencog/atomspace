/*
 * opencog/atoms/base/Atom.h
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Thiago Maia <thiago@vettatech.com>
 *            Andre Senna <senna@vettalabs.com>
 *            Welter Silva <welter@vettalabs.com>
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

#ifndef _OPENCOG_ATOM_H
#define _OPENCOG_ATOM_H

#include <memory>
#include <mutex>
#include <set>
#include <string>

#include <boost/signals2.hpp>

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/ProtoAtom.h>
#include <opencog/truthvalue/AttentionValue.h>
#include <opencog/truthvalue/TruthValue.h>

class AtomUTest;

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

class AtomSpace;

//! arity of Links, represented as short integer (16 bits)
typedef unsigned short Arity;

class Link;
typedef std::shared_ptr<Link> LinkPtr;
typedef std::vector<LinkPtr> IncomingSet; // use vector; see below.
typedef std::weak_ptr<Link> WinkPtr;
typedef std::set<WinkPtr, std::owner_less<WinkPtr> > WincomingSet;
typedef boost::signals2::signal<void (AtomPtr, LinkPtr)> AtomPairSignal;

// We use a std:vector instead of std::set for IncomingSet, because
// virtually all access will be either insert, or iterate, so we get
// O(1) performance. We use std::set for WincomingSet, because we want
// both good insert and good remove performance.  Note that sometimes
// incoming sets can be huge (millions of atoms).

/**
 * Atoms are the basic implementational unit in the system that
 * represents nodes and links. In terms of inheritance, nodes and
 * links are specialization of atoms, that is, they inherit all
 * properties from atoms.
 */
class Atom
    : public ProtoAtom
{
    friend class AtomStorage;     // Needs to set _uuid
    friend class AtomTable;       // Needs to call MarkedForRemoval()
    friend class AtomSpace;       // Needs to call getAtomTable()
    friend class DeleteLink;      // Needs to call getAtomTable()
    friend class Handle;          // Needs to view _uuid
    friend class TLB;             // Needs to view _uuid
    friend class ProtocolBufferSerializer; // Needs to de/ser-ialize an Atom

private:
    //! Sets the AtomTable in which this Atom is inserted.
    void setAtomTable(AtomTable *);

    //! Returns the AtomTable in which this Atom is inserted.
    AtomTable *getAtomTable() const { return _atomTable; }

protected:
    // Byte of bitflags (each bit is a flag, see AtomSpaceDefinites.h)
    // Place this first, so that is shares a word with Type.
    char _flags;

    UUID _uuid;
    AtomTable *_atomTable;

    TruthValuePtr _truthValue;
    AttentionValuePtr _attentionValue;

    // Lock, used to serialize changes.
    // This costs 40 bytes per atom.  Tried using a single, global lock,
    // but there seemed to be too much contention for it, so instead,
    // we are using a lock-per-atom, even though this makes the atom
    // kind-of fat.
    std::mutex _mtx;

    /**
     * Constructor for this class. Protected; no user should call this
     * directly.  Only derived classes (Node, Link) can call this.
     *
     * @param The type of the atom.
     * @param Outgoing set of the atom, that is, the set of atoms this
     * atom references. It must be an empty vector if the atom is a node.
     * @param The truthValue of the atom.
     */
    Atom(Type t, TruthValuePtr tv = TruthValue::DEFAULT_TV(),
            AttentionValuePtr av = AttentionValue::DEFAULT_AV())
      : ProtoAtom(t),
        _flags(0),
        _uuid(Handle::INVALID_UUID),
        _atomTable(NULL),
        _truthValue(tv),
        _attentionValue(av)
    {}

    struct InSet
    {
        // The incoming set is not tracked by the garbage collector;
        // this is required, in order to avoid cyclic references.
        // That is, we use weak pointers here, not strong ones.
        // std::set<ptr> uses 48 bytes (per atom).  See the README file
        // in this directory for a slightly longer explanation for why
        // weak pointers are needed, and why bdgc cannot be used.
        WincomingSet _iset;
#ifdef INCOMING_SET_SIGNALS
        // Some people want to know if the incoming set has changed...
        // However, these make the atom quite fat, so this is disabled
        // just right now. If users really start clamoring, then we can
        // turn this on.
        AtomPairSignal _addAtomSignal;
        AtomPairSignal _removeAtomSignal;
#endif /* INCOMING_SET_SIGNALS */
    };
    typedef std::shared_ptr<InSet> InSetPtr;
    InSetPtr _incoming_set;
    void keep_incoming_set();
    void drop_incoming_set();

    // Insert and remove links from the incoming set.
    void insert_atom(LinkPtr);
    void remove_atom(LinkPtr);

private:
    /** Returns whether this atom is marked for removal.
     *
     * @return Whether this atom is marked for removal.
     */
    bool isMarkedForRemoval() const;

    //! Marks the atom for removal.
    void markForRemoval();

    //! Unsets removal flag.
    void unsetRemovalFlag();

    /** Change the Very-Long-Term Importance */
    void chgVLTI(int unit);

public:

    virtual ~Atom();

    //! Returns the AtomTable in which this Atom is inserted.
    AtomSpace* getAtomSpace() const;

    inline UUID getUUID() const { return _uuid; }

    virtual bool isNode() const = 0;
    virtual bool isLink() const = 0;

    virtual const std::string& getName() const {
        throw RuntimeException(TRACE_INFO, "Not a node!");
    }

    virtual Arity getArity() const {
        throw RuntimeException(TRACE_INFO, "Not a link!");
    }

    virtual const HandleSeq& getOutgoingSet() const {
        throw RuntimeException(TRACE_INFO, "Not a link!");
    }

    virtual Handle getOutgoingAtom(Arity) const {
        throw RuntimeException(TRACE_INFO, "Not a link!");
    }

    /** Returns the handle of the atom.
     *
     * @return The handle of the atom.
     */
    inline Handle getHandle() {
        return Handle(std::dynamic_pointer_cast<Atom>(shared_from_this()));
    }

    /** Returns the AttentionValue object of the atom.
     *
     * @return The pointer to the AttentionValue object
     * of the atom.
     */
    AttentionValuePtr getAttentionValue();

    //! Sets the AttentionValue object of the atom.
    void setAttentionValue(AttentionValuePtr);

    /// Handy-dandy convenience getters for attention values.
    AttentionValue::sti_t getSTI()
    {
        return getAttentionValue()->getSTI();
    }

    AttentionValue::lti_t getLTI()
    {
        return getAttentionValue()->getLTI();
    }

    AttentionValue::vlti_t getVLTI()
    {
        return getAttentionValue()->getVLTI();
    }

    /** Change the Short-Term Importance */
    void setSTI(AttentionValue::sti_t stiValue)
    {
        /* Make a copy */
        AttentionValuePtr old_av = getAttentionValue();
        AttentionValuePtr new_av = createAV(
            stiValue,
            old_av->getLTI(),
            old_av->getVLTI());
        setAttentionValue(new_av);
    }

    /** Change the Long-Term Importance */
    void setLTI(AttentionValue::lti_t ltiValue)
    {
        AttentionValuePtr old_av = getAttentionValue();
        AttentionValuePtr new_av = createAV(
            old_av->getSTI(),
            ltiValue,
            old_av->getVLTI());
        setAttentionValue(new_av);
    }

    /** Change the Very-Long-Term Importance */
    void setVLTI(AttentionValue::vlti_t vltiValue)
    {
        AttentionValuePtr old_av = getAttentionValue();
        AttentionValuePtr new_av = createAV(
            old_av->getSTI(),
            old_av->getLTI(),
            vltiValue);
        setAttentionValue(new_av);
    }

    /** Increase the Very-Long-Term Importance by 1 */
    void incVLTI() { chgVLTI(+1); }

    /** Decrease the Very-Long-Term Importance by 1 */
    void decVLTI() { chgVLTI(-1); }

    /** Returns the TruthValue object of the atom.
     *
     * @return The const referent to the TruthValue object of the atom.
     */
    TruthValuePtr getTruthValue();

    //! Sets the TruthValue object of the atom.
    void setTruthValue(TruthValuePtr);

    /** merge truth value into this */
    void merge(TruthValuePtr, const MergeCtrl& mc=MergeCtrl());

    /**
     * Merge truth value, return Handle for this.
     * This allows oneliners such as:
     *   Handle h = atomSpace->addNode(FOO_NODE, "foo")->tvmerge(tv);
     */
    inline Handle tvmerge(TruthValuePtr tv) {
        merge(tv);
        return getHandle();
    }

    //! Get the size of the incoming set.
    size_t getIncomingSetSize();

    //! Return the incoming set of this atom.
    //! If the AtomSpace pointer is non-null, then only those atoms
    //! that belonged to that atomspace at the time this call was made
    //! are returned. Otherwise, the entire incoming set is returned.
    //!
    //! This call is thread-safe again simultaneous deletion of atoms.
    //! That is, this call returns the incoming set as it was att the
    //! time of the call; any deletions that occur afterwards (possibly
    //! in other threads) will not be reflected in the returned set.
    IncomingSet getIncomingSet(AtomSpace* = NULL);

    //! Place incoming set into STL container of Handles.
    //! Example usage:
    //!     std::vector<Handle> hvect;
    //!     atom->getIncomingSet(back_inserter(hvect));
    //! The resulting vector hvect will contain only valid handles
    //! that were actually part of the incoming set at the time of
    //! the call to this function.
    template <typename OutputIterator> OutputIterator
    getIncomingSet(OutputIterator result)
    {
        if (NULL == _incoming_set) return result;
        std::lock_guard<std::mutex> lck(_mtx);
        // Sigh. I need to compose copy_if with transform. I could
        // do this wih boost range adaptors, but I don't feel like it.
        auto end = _incoming_set->_iset.end();
        for (auto w = _incoming_set->_iset.begin(); w != end; w++)
        {
            Handle h(w->lock());
            if (h) { *result = h; result ++; }
        }
        return result;
    }

    //! Invoke the callback on each atom in the incoming set of
    //! handle h, until one of them returns true, in which case
    //! iteration stopsm and true is returned. Otherwise the
    //! callback is called on all incomings and false is returned.
    template<class T>
    inline bool foreach_incoming(bool (T::*cb)(const Handle&), T *data)
    {
        // We make a copy of the set, so that we don't call the
        //callback with locks held.
        IncomingSet vh(getIncomingSet());

        for (const LinkPtr& lp : vh)
            if ((data->*cb)(Handle(lp))) return true;
        return false;
    }

    /**
     * Returns the set of atoms with a given target handle in their
     * outgoing set (atom type and its subclasses optionally).
     * That is, returns the incoming set of Handle h, with some optional
     * filtering.
     *
     * @param The handle that must be in the outgoing set of the atom.
     * @param The optional type of the atom.
     * @param Whether atom type subclasses should be considered.
     * @return The set of atoms of the given type with the given handle
     *         in their outgoing set.
     */
    template <typename OutputIterator> OutputIterator
    getIncomingSetByType(OutputIterator result,
                         Type type, bool subclass = false)
    {
        if (NULL == _incoming_set) return result;
        std::lock_guard<std::mutex> lck(_mtx);
        ClassServer& cs(classserver());
        // Sigh. I need to compose copy_if with transform. I could
        // do this wih boost range adaptors, but I don't feel like it.
        auto end = _incoming_set->_iset.end();
        for (auto w = _incoming_set->_iset.begin(); w != end; w++)
        {
            Handle h(w->lock());
            if (nullptr == h) continue;
            Type at(h->getType());
            if (type == at or (subclass and cs.isA(at, type))) {
                *result = h;
                result ++;
            }
        }
        return result;
    }

    /**
     * Functional version of getIncomingSetByType
     */
    IncomingSet getIncomingSetByType(Type type, bool subclass = false);

    /** Returns a string representation of the node.
     *
     * @return A string representation of the node.
     * cannot be const, because observing the TV and AV requires a lock.
     */
    virtual std::string toString(const std::string& indent) = 0;
    virtual std::string toShortString(const std::string& indent) = 0;

    // Work around gdb's incapability to build a string on the fly,
    // see http://stackoverflow.com/questions/16734783 for more
    // explanation.
    std::string toString() { return toString(""); }
    std::string toShortString() { return toShortString(""); }

    /** Returns whether two atoms are equal.
     *
     * @return true if the atoms are equal, false otherwise.
     */
    virtual bool operator==(const Atom&) const = 0;

    /** Returns whether two atoms are different.
     *
     * @return true if the atoms are different, false otherwise.
     */
    bool operator!=(const Atom& other) const
    { return not operator==(other); }

    virtual bool operator==(const ProtoAtom& other) const
    {
        if (_type != other.getType()) return false;
        return operator==(dynamic_cast<const Atom&>(other));
    }

    /** Returns whether this atom is less than the given atom.
     *
     * @return true if this atom is less than the given one, false otherwise.
     */
    virtual bool operator<(const Atom&) const = 0;
};

static inline AtomPtr AtomCast(const ProtoAtomPtr& pa)
    { return std::dynamic_pointer_cast<Atom>(pa); }

static inline AtomPtr AtomCast(const Handle& h)
    { return AtomPtr(h); }

static inline Handle HandleCast(const ProtoAtomPtr& pa)
    { return Handle(AtomCast(pa)); }


/** @}*/
} // namespace opencog

#endif // _OPENCOG_ATOM_H
