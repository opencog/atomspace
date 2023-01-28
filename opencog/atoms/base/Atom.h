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

#include <atomic>
#include <functional>
#include <memory>
#include <shared_mutex>
#include <string>
#include <unordered_set>

#if HAVE_FOLLY
#include <folly/container/F14Set.h>
#define USE_HASHABLE_WEAK_PTR 1
#endif

#include <opencog/util/empty_string.h>
#include <opencog/util/sigslot.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/truthvalue/TruthValue.h>

#define INCOMING_SHARED_LOCK std::shared_lock<std::shared_mutex> lck(_mtx);
#define INCOMING_UNIQUE_LOCK std::unique_lock<std::shared_mutex> lck(_mtx);
#define KVP_UNIQUE_LOCK std::unique_lock<std::shared_mutex> lck(_mtx);
#define KVP_SHARED_LOCK std::shared_lock<std::shared_mutex> lck(_mtx);

namespace opencog
{
#if USE_HASHABLE_WEAK_PTR
template<class T>
struct hashable_weak_ptr : public std::weak_ptr<T>
{
	hashable_weak_ptr(std::shared_ptr<T>const& sp) :
		std::weak_ptr<T>(sp)
	{
		if (!sp) return;
		_hash = std::hash<T*>{}(sp.get());
	}

	std::size_t get_hash() const noexcept { return _hash; }

	// Define operator<() in order to construct operator==()
	// It might be more efficient to store the unhashed
	// pointer, and use that for equality compares...
	friend bool operator<(hashable_weak_ptr const& lhs, hashable_weak_ptr const& rhs)
	{
		return lhs.owner_before(rhs);
	}
	friend bool operator!=(hashable_weak_ptr const& lhs, hashable_weak_ptr const& rhs)
	{
		return lhs<rhs or rhs<lhs;
	}
	friend bool operator==(hashable_weak_ptr const& lhs, hashable_weak_ptr const& rhs)
	{
		return not (lhs != rhs);
	}
	private:
		std::size_t _hash = 0;
};

typedef hashable_weak_ptr<Atom> WinkPtr;

#else // USE_HASHABLE_WEAK_PTR

// See discussion in README, explaining why using a bare pointer is safe.
//
// Based on current measurements (March 2022, benchmark/query-loop/diary.txt)
// there is no performance advantage to using bare pointers. In addition,
// it appears that AtomSpaceAsyncUTest fails, probably due to "trivial"
// reasons. Thus, there does not seem to be any advantage to enabling bare
// pointers, and perhaps some minor disadvantages.
// #define USE_BARE_BACKPOINTER 1
//
#if USE_BARE_BACKPOINTER
typedef const Atom* WinkPtr;
#else // USE_BARE_BACKPOINTER
typedef std::weak_ptr<Atom> WinkPtr;
#endif // USE_BARE_BACKPOINTER

#endif // USE_HASHABLE_WEAK_PTR
}

#if USE_BARE_BACKPOINTER
	#define WEAKLY_DO(HA,WP,STMT) { Handle HA(WP->get_handle()); STMT; }
#else  // USE_BARE_BACKPOINTER
	#define WEAKLY_DO(HA,WP,STMT) { Handle HA(WP.lock()); if (HA) { STMT; }}
#endif // USE_BARE_BACKPOINTER

namespace std
{

#if USE_HASHABLE_WEAK_PTR
template<class T> struct owner_less<opencog::hashable_weak_ptr<T>>
{
	bool operator()(const opencog::hashable_weak_ptr<T>& lhs,
	                const opencog::hashable_weak_ptr<T>& rhs) const noexcept
	{
		return lhs.owner_before(rhs);
	}
};

template<class T> struct hash<opencog::hashable_weak_ptr<T>>
{
	std::size_t operator()(const opencog::hashable_weak_ptr<T>& w) const noexcept
	{
		return w.get_hash();
	}
};
#else // USE_HASHABLE_WEAK_PTR

#if USE_BARE_BACKPOINTER
template <> struct owner_less<const opencog::Atom*>
{
	bool operator()(const opencog::Atom* const& lhs,
	                const opencog::Atom* const& rhs) const noexcept
	{
		return lhs < rhs;
	}
};
#endif //USE_BARE_BACKPOINTER

#endif // USE_HASHABLE_WEAK_PTR


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

#if HAVE_FOLLY
// typedef folly::F14ValueSet<WinkPtr, std::owner_hash<WinkPtr> > WincomingSet;
typedef folly::F14ValueSet<WinkPtr> WincomingSet;
#else
// typedef std::unordered_set<WinkPtr> WincomingSet;
typedef std::set<WinkPtr, std::owner_less<WinkPtr> > WincomingSet;
#endif

/**
 * Atoms are the basic implementational unit in the system that
 * represents nodes and links. In terms of C++ inheritance, Nodes and
 * Links are specializations of Atoms, that is, they inherit all
 * properties from Atoms.
 *
 * A "typical" atom is about 500 Bytes in size. The RAM usage breakdown is as
 * follows:
 * -- 24 Bytes std::enable_shared_from_this<Value>
 * --  8 Bytes Type _type plus 4 bool flags.
 * --  8 Bytes ContentHash _content_hash;
 * --  8 Bytes AtomSpace *_atom_space;
 * -- 48 Bytes std::map<const Handle, ValuePtr> _values;
 * -- 56 Bytes std::shared_mutex _mtx;
 * -- 48 Bytes std::map<Type, WincomingSet> _incoming_set;
 * Total: 200 Bytes for a base naked Atom.
 *
 * Node: Additional 32 Bytes for std::string _name + sizeof(chars of string)
 * Link: Additional 24 Bytes for std::vector _outgoing + 16*(_outgoing.size());
 *       A "typical" Link of size 2 is 256 Bytes, outside of AtomSpace
 *
 * Inserted into the AtomSpace: ?? per hash bucket. I guess 24 or 32
 * Per addition to incoming set: 64 per std::_Rb_tree node
 * Per non-default truth value, e.g. CountTruthValue:
 * -- 24 Bytes std::enable_shared_from_this<Value>
 * --  8 Bytes Type _type plus padding
 * -- 24 Bytes std::vector<double> _value
 * -- 24 Bytes 3*sizeof(double)
 * -- 64 std::_Rb_tree node in the Atom holding the TV
 * Total: 144 Bytes per CountTV (ouch).
 *
 * A "typical" Link of size 2, held in one other Link, in AtomSpace, holding
 *   a CountTV in it: 496 Bytes.  This is indeed what is measured in real-life
 *   large datasets.
 */
class Atom
    : public Value
{
    friend class AtomSpace;       // Needs to setChecked
    friend class TypeIndex;       // Needs to clear _atom_space
    friend class Link;            // Needs to call install_atom()
    friend class Frame;           // Needs to call install_atom()
    friend class StateLink;       // Needs to call swap_atom()
    friend class StorageNode;     // Needs to call isAbsent()

protected:
    // Each atomic_flag chews up a byte.
    // Place this first, so that these share a word with Type.
    mutable std::atomic_bool _absent;
    mutable std::atomic_bool _marked_for_removal;
    mutable std::atomic_bool _checked;
    mutable bool _use_iset;

    /// Merkle-tree hash of the atom contents. Generically useful
    /// for indexing and comparison operations.
    mutable ContentHash _content_hash;

    //! Sets the AtomSpace in which this Atom is inserted.
    virtual void setAtomSpace(AtomSpace *);

    AtomSpace *_atom_space;

    /// All of the values on the atom, including the TV.
    mutable std::map<const Handle, ValuePtr> _values;

    // Lock, used to serialize changes.
    // This costs 56 bytes per atom.  Tried using a single, global lock,
    // but there seemed to be too much contention for it, so instead,
    // we are using a lock-per-atom, even though this makes the atom
    // fatter.
    mutable std::shared_mutex _mtx;

    /**
     * Constructor for this class. Protected; no user should call this
     * directly.  Only derived classes (Node, Link) can call this.
     *
     * @param The type of the atom.
     */
    Atom(Type t)
      : Value(t),
        _absent(false),
        _marked_for_removal(false),
        _checked(false),
        _use_iset(false),
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
        // be the source of bottlenecks.  Note that an atomspace can
        // contain a hundred-million atoms, so the solution has to be
        // small. This rules out using a vector to store the
        // buckets (I tried).
        std::map<Type, WincomingSet> _iset;
    };
    InSet _incoming_set;
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
    //! Returns whether this atom is marked for removal.
    bool isMarkedForRemoval() const;

    //! Marks the atom for removal. Returns old value.
    bool markForRemoval();

    //! Unsets removal flag. Returns old value.
    bool unsetRemovalFlag();

    /** Returns whether this atom is marked checked. */
    bool isChecked() const;
    bool setChecked();
    bool setUnchecked();

    /** Returns whether this atom is marked absent. */
    bool isAbsent() const;
    bool setAbsent();
    bool setPresent();

    void getLocalInc(const AtomSpace*, HandleSet&, Type) const;
    void getCoveredInc(const AtomSpace*, HandleSet&, Type) const;

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

    /// Increment the CountTruthValue atomically.
    /// Return the new TruthValue
    TruthValuePtr incrementCountTV(double);

    /// Associate `value` to `key` for this atom.
    void setValue(const Handle& key, const ValuePtr& value);
    /// Get value at `key` for this atom.
    ValuePtr getValue(const Handle& key) const;
    /// Atomically increment a generic FloatValue.
    ValuePtr incrementCount(const Handle& key, const std::vector<double>&);
    ValuePtr incrementCount(const Handle& key, size_t idx, double);

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

    /// Remove all values. The only anticipated users of this are the
    // storage backends, manipulating multi-AtomSpace bulk loads.
    void clearValues();

    //! Return true if the incoming set is empty.
    bool isIncomingSetEmpty(const AtomSpace* = nullptr) const;

    //! Get the size of the incoming set.
    size_t getIncomingSetSize(const AtomSpace* = nullptr) const;

    //! Return the incoming set of this atom.
    //! If the AtomSpace pointer is non-null, then only those atoms
    //! that belonged to that atomspace at the time this call was made
    //! are returned. Otherwise, the entire incoming set is returned.
    //!
    //! This call is thread-safe against simultaneous deletion of atoms.
    //! That is, this call returns the incoming set as it was at the
    //! time of the call; any deletions that occur afterwards (possibly
    //! in other threads) will not be reflected in the returned set.
    IncomingSet getIncomingSet(const AtomSpace* = nullptr) const;

    /** Functional version of getIncomingSetByType.  */
    IncomingSet getIncomingSetByType(Type, const AtomSpace* = nullptr) const;

    /** Return the size of the incoming set, for the given type. */
    size_t getIncomingSetSizeByType(Type, const AtomSpace* = nullptr) const;

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

    // ---------------------------------------------------------
    // Deprecated calls, do not use these in new code!
    // Some day, these will be removed.

    //! Deprecated! Do not use in new code!
    //! Place incoming set into STL container of Handles.
    //! Example usage:
    //!     HandleSeq hvect;
    //!     atom->getIncomingSet(back_inserter(hvect));
    //! The resulting vector hvect will contain only valid handles
    //! that were actually part of the incoming set at the time of
    //! the call to this function.
    template <typename OutputIterator> OutputIterator
    getIncomingIter(OutputIterator result) const
    {
        if (not _use_iset) return result;
        INCOMING_SHARED_LOCK;
        for (const auto& bucket : _incoming_set._iset)
        {
            for (const WinkPtr& w : bucket.second)
                WEAKLY_DO(h, w, { *result = h; result ++; })
        }
        return result;
    }

    //! Deprecated! Do not use in new code!
    //! Invoke the callback on each atom in the incoming set of
    //! handle h, until one of them returns true, in which case
    //! iteration stops, and true is returned. Otherwise the
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
     * Deprecated! Do not use in new code!
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
        if (not _use_iset) return result;
        INCOMING_SHARED_LOCK;

        const auto bucket = _incoming_set._iset.find(type);
        if (bucket == _incoming_set._iset.cend()) return result;

        for (const WinkPtr& w : bucket->second)
            WEAKLY_DO(h, w, { *result = h; result ++; })
        return result;
    }
};

#define ATOM_PTR_DECL(CNAME)                                \
    typedef std::shared_ptr<CNAME> CNAME##Ptr;              \
    static inline CNAME##Ptr CNAME##Cast(const Handle& h)   \
        { return std::dynamic_pointer_cast<CNAME>(h); }     \
    static inline CNAME##Ptr CNAME##Cast(const AtomPtr& a)  \
        { return std::dynamic_pointer_cast<CNAME>(a); }

#define CREATE_DECL(CNAME)  std::make_shared<CNAME>

static inline AtomPtr AtomCast(const ValuePtr& pa)
    { return std::dynamic_pointer_cast<Atom>(pa); }

static inline AtomPtr AtomCast(const Handle& h)
    { return AtomPtr(h); }

static inline Handle HandleCast(const ValuePtr& pa)
    { return Handle(AtomCast(pa)); }

static inline ValuePtr ValueCast(const Handle& h)
    { return std::dynamic_pointer_cast<Value>(h); }

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
