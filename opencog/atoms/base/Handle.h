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
#include <unordered_set>
#include <vector>

/** \addtogroup grp_atomspace
 *  @{
 */
namespace opencog
{

//! UUID == Universally Unique Identifier
typedef unsigned long UUID;

class Atom;
typedef std::shared_ptr<Atom> AtomPtr;

//! contains an unique identificator
class AtomTable;
class Handle
{

friend class AtomTable;
friend class AtomStorage;         // persistance
friend class AtomspaceHTabler;    // persistance

private:
    AtomPtr _ptr;

    static bool atoms_less(const Atom*, const Atom*);
    static AtomPtr do_res(UUID);
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

    Handle& operator=(const AtomPtr& a) {
        this->_ptr = a;
        return *this;
    }

    inline Atom* operator->() {
        return _ptr.get();
    }

    inline Atom* operator->() const {
        return _ptr.get();
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
    inline bool operator< (const Handle& h) const noexcept {
       return atoms_less(_ptr.get(), h._ptr.get());
    }
    inline bool operator> (const Handle& h) const noexcept {
       return atoms_less(h._ptr.get(), _ptr.get());
    }
    inline bool operator<=(const Handle& h) const noexcept {
       return not atoms_less(h._ptr.get(), _ptr.get());
    }
    inline bool operator>=(const Handle& h) const noexcept {
       return not atoms_less(_ptr.get(), h._ptr.get());
    }

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

    operator AtomPtr() const {
        return _ptr;
    }
    operator AtomPtr() {
        return _ptr;
    }
/***
    operator const AtomPtr&() {
        return _ptr;
    }
***/
};

static inline bool operator== (std::nullptr_t, const Handle& rhs) noexcept
    { return rhs == nullptr; }

static inline bool operator!= (std::nullptr_t, const Handle& rhs) noexcept
    { return rhs != nullptr; }

class HandlePredicate {
public:
    inline bool operator()(const Handle& h) const { return this->test(h); }
    virtual bool test(const Handle&) const = 0;
};
class AtomPredicate {
public:
    inline bool operator()(const AtomPtr& a) const { return this->test(a); }
    virtual bool test(const AtomPtr&) const = 0;
};
class AtomComparator {
public:
    inline bool operator()(const AtomPtr& a, const AtomPtr& b) const
        { return this->test(a,b); }
    virtual bool test(const AtomPtr&, const AtomPtr&) const = 0;
};


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

//! a list of handles
typedef std::vector<Handle> HandleSeq;
//! a list of lists of handles
typedef std::vector<HandleSeq> HandleSeqSeq;
//! a hash that associates the handle to its unique identificator
typedef std::unordered_set<Handle, handle_hash> UnorderedHandleSet;

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

} // namespace opencog

namespace std {
inline std::ostream& operator<<(std::ostream& out, const opencog::Handle& h)
{
    out << h.value();
    return out;
}

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

} //namespace std

/** @}*/
#endif // _OPENCOG_HANDLE_H
