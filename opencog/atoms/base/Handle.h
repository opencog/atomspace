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
#include <cstdint>
#include <functional>
#include <limits>
#include <map>
#include <memory>
#include <string>
#include <sstream>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <opencog/util/empty_string.h>
#include <opencog/util/Counter.h>
#include <opencog/atoms/atom_types/types.h>

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
typedef size_t UUID;

//! ContentHash == 64-bit hash of an Atom.
typedef uint64_t ContentHash;

class Atom;
typedef std::shared_ptr<Atom> AtomPtr;

//! Pointer to an Atom, providing extra utility/convenience methods.
class Handle : public AtomPtr
{

friend class Atom;
friend class content_based_atom_ptr_less;
friend class content_based_handle_less;

private:
    static bool atoms_less(const Atom*, const Atom*);
    static bool content_based_atoms_less(const Atom*, const Atom*);

    static const AtomPtr NULL_POINTER;

public:
    static const ContentHash INVALID_HASH = std::numeric_limits<size_t>::max();
    static const Handle UNDEFINED;

    // Copy constructor
    explicit Handle(const AtomPtr& atom) : AtomPtr(atom) {}

    // Move constructor
    explicit Handle(AtomPtr&& atom) : AtomPtr(atom) {}

    explicit Handle() {}

    ~Handle() {}

    ContentHash value(void) const;

    // Copy assign operator
    inline Handle& operator=(const AtomPtr& a) {
        this->AtomPtr::operator=(a);
        return *this;
    }

    // Move assign operator
    inline Handle& operator=(AtomPtr&& a) {
        this->AtomPtr::operator=(a);
        return *this;
    }

    // Cython can't access operator->() so we'll duplicate here.
    inline Atom* atom_ptr() {
        return get();
    }

    inline const Atom* const_atom_ptr() const {
        return get();
    }

    // Allows expressions like "if(h)..." to work
    // when h has a non-null pointer.
    explicit inline operator bool() const noexcept {
        if (get()) return true;
        return false;
    }

    inline bool operator==(std::nullptr_t) const noexcept {
        return get() == nullptr;
    }
    inline bool operator!=(std::nullptr_t) const noexcept {
        return get() != nullptr;
    }
    inline bool operator==(const Atom* ap) const noexcept {
        return get() == ap;
    }
    inline bool operator!=(const Atom* ap) const noexcept {
        return get() != ap;
    }

    inline bool operator==(const Handle& h) const noexcept {
        return get() == h.get();
    }
    inline bool operator!=(const Handle& h) const noexcept {
        return get() != h.get();
    }

    /**
     * Defer to Atom::operator< thus is content based.
     */
    bool operator< (const Handle& h) const noexcept;

    /**
     * Returns a negative value, zero or a positive value if the first
     * argument is respectively smaller than, equal to, or larger than
     * the second argument. The atom hashes are compared, so the
     * comparison is content-based, and is stable, independent of the
     * the address space layout.
     *
     * @param The first handle element.
     * @param The second handle element.
     * @return A negative value, zero or a positive value if the first
     * argument is respectively smaller than, equal to, or larger then the
     * second argument.
     */
    static int compare(const Handle&, const Handle&);
};

static inline bool operator== (std::nullptr_t, const Handle& rhs) noexcept
    { return rhs == (Atom*) nullptr; }

static inline bool operator!= (std::nullptr_t, const Handle& rhs) noexcept
    { return rhs != (Atom*) nullptr; }

bool content_eq(const opencog::Handle& lh,
                const opencog::Handle& rh) noexcept;

//! Boost needs this function to be called by exactly this name.
std::size_t hash_value(Handle const&);

//! a pair of Handles
typedef std::pair<Handle, Handle> HandlePair;

//! a list of handles
typedef std::vector<Handle> HandleSeq;

//! a set of lists of handles
typedef std::set<HandleSeq> HandleSeqSet;

//! a list of lists of handles
typedef std::vector<HandleSeq> HandleSeqSeq;

//! a set of handles. Default handle set container. Usually takes less
//! RAM and has faster iteration.
typedef std::set<Handle> HandleSet;

//! a set of sets of handles.
typedef std::set<HandleSet> HandleSetSet;

//! a sequence of sets of handles.
typedef std::vector<HandleSet> HandleSetSeq;

//! a hash table. Usually has faster insertion.
typedef std::unordered_set<Handle> UnorderedHandleSet;

//! an ordered map from Handle to Handle
typedef std::map<Handle, Handle> HandleMap;

//! a hash table. Usually has faster insertion.
typedef std::unordered_map<Handle, Handle> UnorderedHandleMap;

//! an ordered map from Handle to HandleSet
typedef std::map<Handle, HandleSet> HandleMultimap;

//! an ordered map from Handle to HandleSeq
typedef std::map<Handle, HandleSeq> HandleSeqMap;

//! a sequence of ordered handle maps
typedef std::vector<HandleMap> HandleMapSeq;

//! a sequence of sequences of ordered handle maps
typedef std::vector<HandleMapSeq> HandleMapSeqSeq;

//! a set of ordered handle maps
typedef std::set<HandleMap> HandleMapSet;

//! a sequence of handle pairs
typedef std::vector<HandlePair> HandlePairSeq;

//! a map from handle to double
typedef Counter<Handle, double> HandleCounter;

//! a map from handle to unsigned
typedef Counter<Handle, unsigned> HandleUCounter;

// A map of variables to thier groundings.  Everyone working with
// groundings uses this type; changing the type here allows easy
// comparisons of performance for these two mapping styles.
// At this time (Dec 2019; gcc-8.3.0) there seems to be no difference
// in performance in the pattern matcher as a result of using the
// unordered aka std::_Hashtable variant vs the std::_Rb_tree variant.
// (as measured with the `guile -l nano-en.scm` benchmark.)
typedef HandleMap GroundingMap;
// typedef UnorderedHandleMap GroundingMap;
typedef std::vector<GroundingMap> GroundingMapSeq;
typedef std::vector<GroundingMapSeq> GroundingMapSeqSeq;

//! a handle iterator
typedef std::iterator<std::forward_iterator_tag, Handle> HandleIterator;

bool content_eq(const opencog::HandleSeq& lhs,
                const opencog::HandleSeq& rhs);

bool content_eq(const opencog::HandleSet& lhs,
                const opencog::HandleSet& rhs);

bool content_eq(const opencog::HandleSetSeq& lhs,
                const opencog::HandleSetSeq& rhs);

//! Check if hs contains h using content_eq as equality.  hs is not
//! assumed to be sorted, thus the complexity is up to linear with the
//! size hs.
bool content_contains(const opencog::HandleSeq& hs, const opencog::Handle& h);

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

//! Content based HandleSet less than operator
struct handle_seq_less
{
    bool operator()(const HandleSeq& hsl, const HandleSeq& hsr) const
    {
        size_t sl = hsl.size();
        size_t sr = hsr.size();
        if (sl != sr) return sl < sr;
        for (size_t i=0; i<sl; i++)
        {
            if (hsl[i] != hsl[i]) return hsl[i].operator<(hsr[i]);
        }
        return false;
    }
};

//! Content based HandleSet pointer less than operator
struct handle_seq_ptr_less
{
    bool operator()(const HandleSeq* hsl, const HandleSeq* hsr) const
    {
        return handle_seq_less().operator()(*hsl, *hsr);
    }
};

//! append string representation of the Hash to the string
static inline std::string operator+ (const char *lhs, Handle h)
{
    std::string rhs = lhs;
    char buff[25];
    // The cast to (unsigned long long) is for 32-bit arches
    snprintf(buff, 24, "%llu)", (unsigned long long) h.value());
    return rhs + buff;
}

//! append string representation of the Hash to the string
static inline std::string operator+ (const std::string &lhs, Handle h)
{
    char buff[25];
    // The cast to (unsigned long long) is for 32-bit arches
    snprintf(buff, 24, "%llu)", (unsigned long long) h.value());
    return lhs + buff;
}

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
#define OC_TO_STRING_INDENT "  "
std::string oc_to_string(const Handle& h,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandlePair& hp,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleSeq& hs,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleSeqSeq& hss,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleSet& ohs,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleSetSet& ohss,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleSetSeq& ohss,
                         const std::string& indent=empty_string);
std::string oc_to_string(const UnorderedHandleSet& uhs,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleMap& hm,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleMap::value_type& hmv,
                         const std::string& indent=empty_string);
std::string oc_to_string(const UnorderedHandleMap& hm,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleMultimap& hmm,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleSeqMap& hsm,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleMapSeq& hms,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleMapSeqSeq& hmss,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleMapSet& hms,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandlePairSeq& hps,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleCounter& hc,
                         const std::string& indent=empty_string);
std::string oc_to_string(const HandleUCounter& huc,
                         const std::string& indent=empty_string);
std::string oc_to_string(Type type,
                         const std::string& indent=empty_string);
std::string oc_to_string(const TypeSet& types,
                         const std::string& indent=empty_string);
std::string oc_to_string(const AtomPtr& aptr,
                         const std::string& indent=empty_string);

/**
 * Cast Handle to the specific Atom subclass. This function is defined only
 * for T which are subclasses of Atom.
 */
template<typename T>
static inline
typename std::enable_if< std::is_base_of<Atom, T>::value, std::shared_ptr<T> >::type
CastFromHandle(const Handle& handle)
{
	return std::dynamic_pointer_cast<T>(handle);
}

/**
 * Cast AtomPtr to the specific Atom subclass. This function is defined only
 * for T which are subclasses of Atom.
 */
template<typename T>
static inline
typename std::enable_if< std::is_base_of<Atom, T>::value, std::shared_ptr<T> >::type
CastFromAtomPtr(const AtomPtr& atom)
{
	return std::dynamic_pointer_cast<T>(atom);
}

} // namespace opencog

namespace std {
ostream& operator<<(ostream&, const opencog::HandleMap&);
ostream& operator<<(ostream&, const opencog::HandleSeq&);
ostream& operator<<(ostream&, const opencog::HandleSet&);
ostream& operator<<(ostream&, const opencog::UnorderedHandleSet&);
ostream& operator<<(ostream&, const opencog::UnorderedHandleMap&);

// Hash, needed for std::unordered_map
template<>
struct hash<opencog::Handle>
{
    typedef std::size_t result_type;
    typedef opencog::Handle argument_type;
    std::size_t operator()(const opencog::Handle& h) const noexcept
    { return hash_value(h); }
};

// content-based equality
template<>
struct equal_to<opencog::Handle>
{
    typedef bool result_type;
    typedef opencog::Handle first_argument;
    typedef opencog::Handle second_argument;
    bool
    operator()(const opencog::Handle& lh,
               const opencog::Handle& rh) const noexcept
    {
        if (lh == rh) return true;
        if (nullptr == lh or nullptr == rh) return false;
        return opencog::content_eq(lh, rh);
    }
};

template<>
struct hash<opencog::HandlePair>
{
    typedef std::size_t result_type;
    typedef opencog::HandlePair argument_type;
    std::size_t
    operator()(const opencog::HandlePair& hp) const noexcept
    { return hash_value(hp.first) + hash_value(hp.second); }
};

// content-based equality
template<>
struct equal_to<opencog::HandlePair>
{
    typedef bool result_type;
    typedef opencog::HandlePair first_argument;
    typedef opencog::HandlePair second_argument;
    bool
    operator()(const opencog::HandlePair& lhp,
               const opencog::HandlePair& rhp) const noexcept
    {
        if (lhp == rhp) return true;
        std::equal_to<opencog::Handle> eq;
        return eq.operator()(lhp.first, rhp.first) and
               eq.operator()(lhp.second, rhp.second);
    }
};

template<>
struct hash<opencog::HandleSeq>
{
    typedef std::size_t result_type;
    typedef opencog::HandleSeq argument_type;
    std::size_t
    operator()(const opencog::HandleSeq& hseq) const noexcept
    {
        std::size_t hsh = 0;
        for (const opencog::Handle& h : hseq) hsh += hash_value(h);
        return hsh;
    }
};

// content-based equality
template<>
struct equal_to<opencog::HandleSeq>
{
    typedef bool result_type;
    typedef opencog::HandleSeq first_argument;
    typedef opencog::HandleSeq second_argument;
    bool
    operator()(const opencog::HandleSeq& lhs,
               const opencog::HandleSeq& rhs) const noexcept
    {
        if (lhs == rhs) return true;
        size_t len = lhs.size();
        if (rhs.size() != len) return false;
        std::equal_to<opencog::Handle> eq;
        for (size_t i=0; i<len; i++)
        {
            if (not eq.operator()(lhs[i], rhs[i])) return false;
        }
        return true;
    }
};

} // ~namespace std

/** @}*/
#endif // _OPENCOG_HANDLE_H
