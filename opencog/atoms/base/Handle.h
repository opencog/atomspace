/*
 * opencog/atoms/base/Handle.h
 *
 * Copyright (C) 2008-2010 OpenCog Foundation
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2013 Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _OPENCOG_HANDLE_H
#define _OPENCOG_HANDLE_H

#include <iostream>
#include <climits>
#include <functional>
#include <memory>
#include <string>
#include <sstream>
#include <set>
#include <unordered_set>
#include <vector>
#include <map>

#include <opencog/atoms/base/types.h>

// Comment this out if you want to enforce more determinism in the
// AtomSpace. For instance atoms are indexed according to content
// rather address, etc.
// #define REPRODUCIBLE_ATOMSPACE

/** \addtogroup grp_atomspace
 *  @{
 */
namespace opencog
{

//! UUID == Universally Unique Identifier
typedef unsigned long UUID;

class Atom;
class Handle;
typedef std::shared_ptr<Atom> AtomPtr;
class AtomTable;
class Link;
typedef std::shared_ptr<Link> LinkPtr;

//! contains an unique identificator
class Handle
{

friend class Atom;
friend class AtomTable;
friend class AtomStorage;         // persistance
friend class content_based_atom_ptr_less;
friend class content_based_handle_less;

private:
    AtomPtr _ptr;

    static bool atoms_less(const Atom*, const Atom*);
    static bool content_based_atoms_less(const Atom*, const Atom*);
    static Handle do_res(UUID);
    static std::vector<const AtomTable*> _resolver;

    static void set_resolver(const AtomTable*);
    static void clear_resolver(const AtomTable*);

    static const AtomPtr NULL_POINTER;
public:

    static const UUID INVALID_UUID = ULONG_MAX;
    static const Handle UNDEFINED;

    explicit Handle(const AtomPtr& atom) : _ptr(atom) {}
    explicit Handle(const UUID);
    explicit Handle() {}
    Handle(const Handle& h) : _ptr(h._ptr) {}
    ~Handle() {}

    UUID value(void) const;

    inline Handle& operator=(const Handle& h) {
        this->_ptr = h._ptr;
        return *this;
    }

    inline Handle& operator=(const AtomPtr& a) {
        this->_ptr = a;
        return *this;
    }

    inline Atom* operator->() {
        return _ptr.get();
    }

    inline Atom* operator->() const {
        return _ptr.get();
    }

#ifdef INLINE_POINTER_CHASING
    Handle& operator=(const AtomPtr& a) {
        this->_ptr = std::dynamic_pointer_cast<Atom>(a);
        return *this;
    }
#endif

    operator AtomPtr() const {
        return _ptr;
    }
    operator AtomPtr() {
        return _ptr;
    }

    // Cython can't access operator->() so we'll duplicate here.
    inline Atom* atom_ptr() {
        return _ptr.get();
    }

    inline const Atom* const_atom_ptr() const {
        return _ptr.get();
    }

    inline bool is_defined() const {
        return *this != Handle::UNDEFINED;
    }

    inline bool is_undefined() const {
        return *this == Handle::UNDEFINED;
    }

    // Allows expressions like "if(h)..." to work when h has a non-null pointer.
    explicit inline operator bool() const noexcept {
        if (_ptr.get()) return true;
        return false;
    }

    inline bool operator==(const Atom* ap) const noexcept {
        return _ptr.get() == ap;
    }
    inline bool operator!=(const Atom* ap) const noexcept {
        return _ptr.get() != ap;
    }

    inline bool operator==(const Handle& h) const noexcept {
        return _ptr.get() == h._ptr.get();
    }
    inline bool operator!=(const Handle& h) const noexcept {
        return _ptr.get() != h._ptr.get();
    }
#ifdef REPRODUCIBLE_ATOMSPACE
#define DEFAULT_ATOMS_LESS content_based_atoms_less
#else
#define DEFAULT_ATOMS_LESS atoms_less
#endif
    inline bool operator< (const Handle& h) const noexcept {
       return DEFAULT_ATOMS_LESS(_ptr.get(), h._ptr.get());
    }
    inline bool operator> (const Handle& h) const noexcept {
       return DEFAULT_ATOMS_LESS(h._ptr.get(), _ptr.get());
    }
    inline bool operator<=(const Handle& h) const noexcept {
       return not DEFAULT_ATOMS_LESS(h._ptr.get(), _ptr.get());
    }
    inline bool operator>=(const Handle& h) const noexcept {
       return not DEFAULT_ATOMS_LESS(_ptr.get(), h._ptr.get());
    }
#undef DEFAULT_ATOMS_LESS

    /**
     * Returns a negative value, zero or a positive value if the first
     * argument is respectively smaller than, equal to, or larger than
     * the second argument.
     *
     * @param The first handle element.
     * @param The second handle element.
     * @return A negative value, zero or a positive value if the first
     * argument is respectively smaller than, equal to, or larger then the
     * second argument.
     */
    static int compare(const Handle& h1, const Handle& h2)
    {
        if (h1 < h2) return -1;
        if (h1 > h2) return 1;
        return 0;
    }
};

static inline bool operator== (std::nullptr_t, const Handle& rhs) noexcept
    { return rhs == nullptr; }

static inline bool operator!= (std::nullptr_t, const Handle& rhs) noexcept
    { return rhs != nullptr; }

//! gcc-4.7.2 needs this, because std::hash<opencog::Handle> no longer works.
//! (See very bottom of this file).
struct handle_hash : public std::unary_function<Handle, size_t>
{
   size_t operator()(const Handle& h) const
   {
       return static_cast<std::size_t>(h.value());
   }
};

//! Boost needs this function to be called by exactly this name.
inline std::size_t hash_value(Handle const& h)
{
    return static_cast<std::size_t>(h.value());
}

struct handle_less
{
   bool operator()(const Handle& hl, const Handle& hr) const
   {
       return hl < hr;
   }
};

//! a pair of Handles
typedef std::pair<Handle, Handle> HandlePair;

//! a list of handles
typedef std::vector<Handle> HandleSeq;

//! a list of lists of handles
typedef std::vector<HandleSeq> HandleSeqSeq;

//! a set of handles
typedef std::set<Handle> OrderedHandleSet;

//! a pair of handles
typedef std::pair<Handle, Handle> HandlePair;

//! a hash that associates the handle to its unique identificator
typedef std::unordered_set<Handle, handle_hash> UnorderedHandleSet;

//! an ordered map from Handle to Handle set
typedef std::map<Handle, UnorderedHandleSet> HandleMultimap;

//! an ordered map from Handle to Handle
typedef std::map<Handle, Handle> HandleMap;

//! a sequence of ordered handle maps
typedef std::vector<HandleMap> HandleMapSeq;

//! a set of ordered handle maps
typedef std::set<HandleMap> HandleMapSet;

//! a pair of handles
typedef std::pair<Handle, Handle> HandlePair;

//! a sequence of handle pairs
typedef std::vector<HandlePair> HandlePairSeq;

//! a handle iterator
typedef std::iterator<std::forward_iterator_tag, Handle> HandleIterator;

struct content_based_atom_ptr_less
{
    bool operator()(const Atom* al, const Atom* ar) const
    {
        return Handle::content_based_atoms_less(al, ar);
    }
};

struct content_based_handle_less
{
    bool operator()(const Handle& hl, const Handle& hr) const
    {
        return Handle::content_based_atoms_less(hl.const_atom_ptr(),
                                                hr.const_atom_ptr());
    }
};

struct handle_seq_less
{
    bool operator()(const HandleSeq& hsl, const HandleSeq& hsr) const
    {
        size_t sl = hsl.size();
        size_t sr = hsr.size();
        if (sl != sr) return sl < sr;
        for (size_t i=0; i<sl; i++) {
            if (hsl[i] != hsr[i]) return hsl[i] < hsr[i];
        }
        return false;
    }
};

struct handle_seq_ptr_less
{
   bool operator()(const HandleSeq* hsl, const HandleSeq* hsr) const
   {
       size_t sl = hsl->size();
       size_t sr = hsr->size();
       if (sl != sr) return sl < sr;
       for (size_t i=0; i<sl; i++) {
           if (hsl->operator[](i) != hsr->operator[](i))
               return hsl->operator[](i) < hsr->operator[](i);
       }
       return false;
   }
};

//! append string representation of the Hash to the string
static inline std::string operator+ (const char *lhs, Handle h)
{
    std::string rhs = lhs;
    char buff[25];
    snprintf(buff, 24, "%lu)", h.value());
    return rhs + buff;
}

//! append string representation of the Hash to the string
static inline std::string operator+ (const std::string &lhs, Handle h)
{
    char buff[25];
    snprintf(buff, 24, "%lu)", h.value());
    return lhs + buff;
}

// Debugging helpers, very convenient to print Handle sets in gdb
std::string h_to_string(const Handle& h);
std::string hp_to_string(const HandlePair& hp);
std::string hs_to_string(const HandleSeq& hs);
std::string ohs_to_string(const OrderedHandleSet& ohs);
std::string uhs_to_string(const UnorderedHandleSet& uhs);
std::string hmap_to_string(const HandleMap& hm);
std::string hmultimap_to_string(const HandleMultimap& hmm);
std::string hmaps_to_string(const HandleMapSeq& hms);
std::string hmapset_to_string(const HandleMapSet& hms);
std::string hps_to_string(const HandlePairSeq& hps);
std::string atomtype_to_string(Type type);
std::string aptr_to_string(const AtomPtr& aptr);
std::string lptr_to_string(const LinkPtr& lptr);

// In case your gdb supports overloading, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const Handle& h);
std::string oc_to_string(const HandlePair& hp);
std::string oc_to_string(const HandleSeq& hs);
std::string oc_to_string(const OrderedHandleSet& ohs);
std::string oc_to_string(const UnorderedHandleSet& uhs);
std::string oc_to_string(const HandleMap& hm);
std::string oc_to_string(const HandleMultimap& hmm);
std::string oc_to_string(const HandleMapSeq& hms);
std::string oc_to_string(const HandleMapSet& hms);
std::string oc_to_string(const HandlePairSeq& hps);
std::string oc_to_string(Type type);
std::string oc_to_string(const AtomPtr& aptr);

} // namespace opencog

namespace std {
ostream& operator<<(ostream&, const opencog::Handle&);
ostream& operator<<(ostream&, const opencog::HandleSeq&);
ostream& operator<<(ostream&, const opencog::OrderedHandleSet&);
ostream& operator<<(ostream&, const opencog::UnorderedHandleSet&);

#ifdef THIS_USED_TO_WORK_GREAT_BUT_IS_BROKEN_IN_GCC472
// The below used to work, but broke in gcc-4.7.2. The reason it
// broke is that it fails to typedef result_type and argument_type,
// which ... somehow used to work automagically?? It doesn't any more.
// I have no clue why gcc-4.7.2 broke this, and neither does google or
// stackoverflow.  You have two choices: use handle_hash, above, or
// cross your fingers, and hope the definition in the #else clause,
// below, works.

template<>
inline std::size_t
std::hash<opencog::Handle>::operator()(opencog::Handle h) const
{
    return static_cast<std::size_t>(h.value());
}

#else

// This works for me, per note immediately above.
template<>
struct hash<opencog::Handle>
{
    typedef std::size_t result_type;
    typedef opencog::Handle argument_type;
    std::size_t
    operator()(opencog::Handle h) const noexcept
    { return static_cast<std::size_t>(h.value()); }
};

#endif // THIS_USED_TO_WORK_GREAT_BUT_IS_BROKEN_IN_GCC472

} // ~namespace std

/** @}*/
#endif // _OPENCOG_HANDLE_H
