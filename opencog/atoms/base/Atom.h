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

#include <functional>
#include <memory>
#include <shared_mutex>
#include <string>
#include <unordered_set>

#include <opencog/util/empty_string.h>
#include <opencog/util/sigslot.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/truthvalue/TruthValue.h>

namespace opencog
{
typedef std::weak_ptr<Atom> WinkPtr;
}

namespace std
{

// The hash of the weak pointer is just the type of the atom.
template<> struct hash<opencog::WinkPtr>
{
    typedef opencog::Type result_type;
    typedef opencog::WinkPtr argument_type;
    opencog::Type operator()(const opencog::WinkPtr& w) const noexcept;
};

// Equality is equality of the underlying atoms. The unordered set
// uses this to distinguish atoms of the same type.
template<> struct equal_to<opencog::WinkPtr>
{
    typedef bool result_type;
    typedef opencog::WinkPtr first_argument;
    typedef opencog::WinkPtr second_argument;
    bool operator()(const opencog::WinkPtr&,
                    const opencog::WinkPtr&) const noexcept;
};

} // namespace std

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

class AtomSpace;

//! arity of Links, represented as size_t to match outcoming set limit
typedef std::size_t Arity;

//! We use a std:vector instead of std::set for IncomingSet, because
//! virtually all access will be either insert, or iterate, so we get
//! O(1) performance. Note that sometimes incoming sets can be huge,
//! millions of atoms.
typedef HandleSeq IncomingSet;
typedef SigSlot<Handle, Handle> AtomPairSignal;

// typedef std::unordered_set<WinkPtr> WincomingSet;
typedef std::set<WinkPtr, std::owner_less<WinkPtr> > WincomingSet;

/**
 * Atoms are the basic implementational unit in the system that
 * represents nodes and links. In terms of C++ inheritance, nodes and
 * links are specializations of atoms, that is, they inherit all
 * properties from atoms.
 */
class Atom
    : public Value
{
    friend class AtomSpace;       // Needs to setChecked
    friend class TypeIndex;       // Needs to clear _atom_space
    friend class Link;            // Needs to call install_atom()
    friend class StateLink;       // Needs to call swap_atom()

protected:
    //! Sets the AtomSpace in which this Atom is inserted.
    virtual void setAtomSpace(AtomSpace *);

    // Byte of bitflags (each bit is a flag).
    // Place this first, so that is shares a word with Type.
    mutable char _flags;

    /// Merkle-tree hash of the atom contents. Generically useful
    /// for indexing and comparison operations.
    mutable ContentHash _content_hash;

    AtomSpace *_atom_space;

    /// All of the values on the atom, including the TV.
    mutable std::map<const Handle, ValuePtr> _values;

    // Lock, used to serialize changes.
    // This costs 40 bytes per atom.  Tried using a single, global lock,
    // but there seemed to be too much contention for it, so instead,
    // we are using a lock-per-atom, even though this makes the atom
    // kind-of fat.
    mutable std::shared_mutex _mtx;

    /**
     * Constructor for this class. Protected; no user should call this
     * directly.  Only derived classes (Node, Link) can call this.
     *
     * @param The type of the atom.
     */
    Atom(Type t)
      : Value(t),
        _flags(0),
        _content_hash(Handle::INVALID_HASH),
        _atom_space(nullptr)
    {}

    Atom& operator=(const Atom& other) // copy assignment operator
        { return *this; }
    Atom& operator=(Atom&& other) // move assignment operator
        { return *this; }

    // The incoming set is not tracked by the garbage collector;
    // this is required, in order to avoid cyclic references.
    // That is, we use weak pointers here, not strong ones.
    // std::set<ptr> uses 48 bytes (per atom).  See the README file
    // in this directory for a slightly longer explanation for why
    // weak pointers are needed, and why bdwgc cannot be used.
    struct InSet
    {
        // We want five things:
        // a) the smallest possiblem atom.
        // b) excellent insert performance.
        // c) very fast lookup by type.
        // d) good remove performance.
        // e) uniqueness, because atomspace operations can sometimes
        //    cause an atom to get inserted multiple times.  This is
        //    arguably a bug, though.
        //
        // In order to get b), we have to store atoms in buckets, each
        // bucket holding only one type.  To satisfy d), the buckets
        // need to be either hash tables or rb-trees. Scanning for
        // uniqueness in a vector is prohibitavely slow.  Note that
        // incoming sets containing 10K atoms are not unusual, and can
        // be the source of bottlnecks.  Note that an atomspace can
        // contain a hundred-million atoms, so the solution has to be
        // small. This rules out using a vector to store the
        // buckets (I tried).
        std::map<Type, WincomingSet> _iset;

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
    void insert_atom(const Handle&);
    void remove_atom(const Handle&);
    void swap_atom(const Handle&, const Handle&);
    virtual void install();
    virtual void remove();

    virtual ContentHash compute_hash() const = 0;

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

    /** Returns whether this atom is marked checked. */
    bool isChecked() const;
    void setChecked();
    void setUnchecked();

public:

    virtual ~Atom();
    virtual bool is_atom() const { return true; }

    //! Returns the AtomSpace in which this Atom is inserted.
    AtomSpace* getAtomSpace() const { return _atom_space; }

    /// Merkle-tree hash of the atom contents. Generically useful
    /// for indexing and comparison operations.
    ///
    /// At this time, we use a 64-bit non-cryptographic hash: it is
    /// just enough to disambiguate most Atoms, but is small enough
    /// that it does not use up a lot of RAM (as AtomSpace algos are
    /// RAM-limited, in practice.)
    ///
    /// If a crypto hash was ever needed, the IPLD hash format would
    /// be recommended.  See https://ipld.io/ for details.
    ///
    /// This hash is NOT stable against modifications of the type
    /// inheritance hierarchy! Changing the type hierarchy might
    /// change the hash, because it will assign a different numeric
    /// value to to the type, which is used in computing the hash.
    /// (this could be avoided by using the string name of the type.)
    ///
    /// This hash is NOT stable against different-sized address spaces,
    /// or even different C++ libraries! That is because it uses the
    /// `std::hash()` function to compute string hashes, and this gives
    /// different answers on 32-bit and 64-bit arches. This could be
    /// fixed by using our own, private string hash function.
    ///
    /// It might be nice to have a hash that is stable against both of
    /// these changes, as it would then enable the comparison of hashes
    /// in a distributed atomspace network. But no one needs this, for
    /// now. (There's no code that sends hashes over the network, or
    /// stores them in a file.)
    inline ContentHash get_hash() const {
        if (Handle::INVALID_HASH != _content_hash)
            return _content_hash;
        _content_hash = compute_hash();
        return _content_hash;
    }

    virtual const std::string& get_name() const {
        throw RuntimeException(TRACE_INFO, "Not a node!");
    }

    virtual Arity get_arity() const { return size(); }

    virtual const HandleSeq& getOutgoingSet() const {
        throw RuntimeException(TRACE_INFO, "Not a link!");
    }

    virtual Handle getOutgoingAtom(Arity) const {
        throw RuntimeException(TRACE_INFO, "Not a link!");
    }

    virtual TruthValuePtr evaluate(AtomSpace*, bool silent=false) {
        throw RuntimeException(TRACE_INFO, "Not evaluatable!");
    }
    virtual bool is_evaluatable() const { return false; }

    virtual ValuePtr execute(AtomSpace*, bool silent=false) {
        throw RuntimeException(TRACE_INFO,
            "Not executable! %s", to_string().c_str());
    }
    virtual ValuePtr execute(void) { return execute(_atom_space, false); }
    virtual bool is_executable() const { return false; }

    /** Returns a handle holding "this". */
    inline Handle get_handle() const {
        return Handle(std::dynamic_pointer_cast<Atom>(
             const_cast<Atom*>(this)->shared_from_this()));
    }

    /** Returns the TruthValue object of the atom. */
    TruthValuePtr getTruthValue() const;

    //! Sets the TruthValue object of the atom.
    void setTruthValue(const TruthValuePtr&);

    /// Associate `value` to `key` for this atom.
    void setValue(const Handle& key, const ValuePtr& value);
    /// Get value at `key` for this atom.
    ValuePtr getValue(const Handle& key) const;

    /// Get the set of all keys in use for this Atom.
    HandleSet getKeys() const;

    /// Copy all the values from the other atom to this one.
    void copyValues(const Handle&);

    /// Return true if the set of values on this atom isn't empty.
    bool haveValues() const {
        // I think its safe to call empty() without holding a lock...!?
        return not _values.empty();
    }

    /// Print all of the key-value pairs.
    std::string valuesToString() const;

    //! Get the size of the incoming set.
    size_t getIncomingSetSize(AtomSpace* = nullptr) const;

    //! Return the incoming set of this atom.
    //! If the AtomSpace pointer is non-null, then only those atoms
    //! that belonged to that atomspace at the time this call was made
    //! are returned. Otherwise, the entire incoming set is returned.
    //!
    //! This call is thread-safe against simultaneous deletion of atoms.
    //! That is, this call returns the incoming set as it was at the
    //! time of the call; any deletions that occur afterwards (possibly
    //! in other threads) will not be reflected in the returned set.
    IncomingSet getIncomingSet(AtomSpace* = nullptr) const;

    //! Place incoming set into STL container of Handles.
    //! Example usage:
    //!     HandleSeq hvect;
    //!     atom->getIncomingSet(back_inserter(hvect));
    //! The resulting vector hvect will contain only valid handles
    //! that were actually part of the incoming set at the time of
    //! the call to this function.
    template <typename OutputIterator> OutputIterator
    getIncomingSet(OutputIterator result) const
    {
        if (nullptr == _incoming_set) return result;
        std::shared_lock<std::shared_mutex> lck(_mtx);
        for (const auto& bucket : _incoming_set->_iset)
        {
            for (const WinkPtr& w : bucket.second)
            {
                Handle h(std::static_pointer_cast<Atom>(w.lock()));
                if (h) { *result = h; result ++; }
            }
        }
        return result;
    }

    //! Invoke the callback on each atom in the incoming set of
    //! handle h, until one of them returns true, in which case
    //! iteration stopsm and true is returned. Otherwise the
    //! callback is called on all incomings and false is returned.
    template<class T>
    inline bool foreach_incoming(bool (T::*cb)(const Handle&), T *data) const
    {
        // We make a copy of the set, so that we don't call the
        // callback with locks held.
        IncomingSet vh(getIncomingSet());

        for (const Handle& lp : vh)
            if ((data->*cb)(lp)) return true;
        return false;
    }

    /**
     * Return all atoms of type `type` that contain this atom.
     * That is, return all atoms that contain this atom, and are
     * also of the given type.
     *
     * @param The iterator where the set of atoms will be returned.
     * @param The type of the parent atom.
     */
    template <typename OutputIterator> OutputIterator
    getIncomingSetByType(OutputIterator result, Type type) const
    {
        if (nullptr == _incoming_set) return result;
        std::shared_lock<std::shared_mutex> lck(_mtx);

        const auto bucket = _incoming_set->_iset.find(type);
        if (bucket == _incoming_set->_iset.cend()) return result;

        for (const WinkPtr& w : bucket->second)
        {
            Handle h(std::static_pointer_cast<Atom>(w.lock()));
            if (h) { *result = h; result ++; }
        }
        return result;
    }

    /** Functional version of getIncomingSetByType.  */
    IncomingSet getIncomingSetByType(Type, AtomSpace* = nullptr) const;

    /** Return the size of the incoming set, for the given type. */
    size_t getIncomingSetSizeByType(Type type, AtomSpace* = nullptr) const;

    /** Returns a string representation of the node. */
    virtual std::string to_string(const std::string& indent) const = 0;
    virtual std::string to_short_string(const std::string& indent) const = 0;
    virtual std::string id_to_string() const;

    // Work around gdb's inability to build a string on the fly,
    // see http://stackoverflow.com/questions/16734783 for more
    // explanation.
    std::string to_string() const { return to_string(""); }
    std::string to_short_string() const { return to_short_string(""); }

    /**
     * Perform a content-based comparison of two atoms.
     * Returns true if the other atom is "semantically" equivalent
     * to this one. Two atoms are semantically equivalent if they
     * accomplish the same thing; even if they differ in details.
     * For example, two ScopeLinks can be semantically equivalent,
     * even though they use different variable names. As long as
     * the different names can be alpha-converted, two different
     * declarations are "the same", and count as the same atom.
     *
     * @return true if the atoms are semantically equal, else false.
     */
    virtual bool operator==(const Atom&) const = 0;

    /** Negation of operator==(). */
    bool operator!=(const Atom& other) const
    { return not operator==(other); }

    virtual bool operator==(const Value& other) const
    {
        if (_type != other.get_type()) return false;
        return operator==(dynamic_cast<const Atom&>(other));
    }

    /** Ordering operator for Atoms. */
    virtual bool operator<(const Atom&) const = 0;
};

static inline AtomPtr AtomCast(const ValuePtr& pa)
    { return std::dynamic_pointer_cast<Atom>(pa); }

static inline AtomPtr AtomCast(const Handle& h)
    { return AtomPtr(h); }

static inline Handle HandleCast(const ValuePtr& pa)
    { return Handle(AtomCast(pa)); }

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
// std::string oc_to_string(const IncomingSet& iset,
//                          const std::string& indent=empty_string);
std::string oc_to_string(const Atom& atom,
                         const std::string& indent=empty_string);

/** @}*/
} // namespace opencog

namespace std {

/// Overload std::less to perform a content-based compare of the
/// AtomPtr's. Otherwise, it seems to just use the address returned
/// by `AtomPtr::get()`. The core problem is that sometimes, were were
/// expecting that `std::less<Handle>` was going to be used, and it
/// wasn't, resulting in an address-space compare. This should halt
/// that misbehavior. See issue #2371 for details.
template<>
struct less<opencog::AtomPtr>
{
    bool operator()(const opencog::AtomPtr& ata, const opencog::AtomPtr& atb) const
    {
        return ata->operator<(*atb);
    }
};

// Overloading operator<< for Incoming Set
ostream& operator<<(ostream&, const opencog::IncomingSet&);

}

#endif // _OPENCOG_ATOM_H
