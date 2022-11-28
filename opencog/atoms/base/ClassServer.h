/*
 * opencog/atoms/base/ClassServer.h
 *
 * Copyright (C) 2011 by The OpenCog Foundation
 * Copyright (C) 2017 by Linas Vepstas
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

#ifndef _OPENCOG_CLASS_SERVER_H
#define _OPENCOG_CLASS_SERVER_H

#include <mutex>
#include <set>
#include <unordered_map>
#include <vector>

#include <opencog/util/sigslot.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Handle.h>

class ClassServerUTest;

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * This class provides factories for those atom types
 * that have non-trivial C++ objects behind them.
 */
class ClassServer
{
    friend class opencog::NameServer;
    friend class ::ClassServerUTest;

public:
    // Currently, we provide factories only for atoms, not for
    // values. TruthValues could use a factory, but, for now,
    // we don't have a pressing reason to add that.
    typedef Handle (AtomFactory)(const Handle&);

    // Perform checking of the outgoing set, during construction.
    typedef bool (Validator)(const Handle&);
private:

    /** Private default constructor for this class to make it a singleton. */
    ClassServer(const NameServer &);

    /* It is very tempting to make the type_mutex into a reader-writer
     * mutex. However, it appears that this is a bad idea: reader-writer
     * mutexes cause cache-line ping-ponging when there is contention,
     * effecitvely serializing access, and are just plain slower when
     * there is no contention.  Thus, the current implementations seem
     * to be a lose-lose proposition. See the Anthony Williams post here:
     * http://permalink.gmane.org/gmane.comp.lib.boost.devel/211180
     */
    mutable std::mutex factory_mutex;

    mutable std::vector<AtomFactory*> _atomFactory;
    mutable std::vector<Validator*> _validator;

    template<typename T>
    void splice(std::vector<T>&, Type, T);

    template<typename T>
    void update(std::vector<T>&, Type);
    void update_factories();

    const NameServer & _nameServer;

    AtomFactory* getFactory(Type) const;

public:
    /** Gets the singleton instance (following meyer's design pattern) */
    friend ClassServer& classserver();

    /**
     * Declare a factory for an atom type.
     */
    void addFactory(Type, AtomFactory*);

    /**
     * Declare a validator for an atom type.
     */
    void addValidator(Type, Validator*);
    Validator* getValidator(Type) const;

    /**
     * Convert the indicated Atom into a C++ instance of the
     * same type.
     */
    Handle factory(const Handle&) const;
};

ClassServer& classserver();

#define TOKENPASTE(x, y) x ## y
#define TOKENPASTE2(x, y) TOKENPASTE(x, y)

#define DEFINE_LINK_FACTORY(CNAME,CTYPE)                          \
                                                                  \
Handle CNAME::factory(const Handle& base)                         \
{                                                                 \
   /* If it's castable, nothing to do. */                         \
   if (CNAME##Cast(base)) return base;                            \
                                                                  \
   Handle h(create##CNAME(std::move(base->getOutgoingSet()),      \
                          base->get_type()));                     \
   return h;                                                      \
}                                                                 \
                                                                  \
/* This runs when the shared lib is loaded. */                    \
/* Set priority to 110 so that it runs after nameserver. */       \
static __attribute__ ((constructor (110))) void                   \
   TOKENPASTE2(init, __COUNTER__)(void)                           \
{                                                                 \
   classserver().addFactory(CTYPE, &CNAME::factory);              \
}

#define DEFINE_NODE_FACTORY(CNAME,CTYPE)                          \
                                                                  \
Handle CNAME::factory(const Handle& base)                         \
{                                                                 \
   if (CNAME##Cast(base)) return base;                            \
   Handle h(create##CNAME(base->get_type(),                       \
                          std::move(base->get_name())));          \
   return h;                                                      \
}                                                                 \
                                                                  \
/* This runs when the shared lib is loaded. */                    \
/* Set priority to 110 so that it runs after nameserver. */       \
static __attribute__ ((constructor (110))) void                   \
   TOKENPASTE2(init, __COUNTER__)(void)                           \
{                                                                 \
   classserver().addFactory(CTYPE, &CNAME::factory);              \
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_CLASS_SERVER_H
