/*
 * opencog/atoms/value/NameServer.h
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

#ifndef _OPENCOG_CLASS_NAMESERVER_H
#define _OPENCOG_CLASS_NAMESERVER_H

#include <mutex>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include <opencog/util/sigslot.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/atom_types/atom_types.h>

class ClassServerUTest;

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

typedef SigSlot<Type> TypeSignal;

/**
 * This class keeps track of the complete protoatom (value and atom)
 * class hierarchy.
 */
class NameServer
{
    friend class ::ClassServerUTest;

private:

    /** Private default constructor for this class to make it a singleton. */
    NameServer();

    std::set< std::string > _loaded_modules;

    /* It is very tempting to make the type_mutex into a reader-writer
     * mutex. However, it appears that this is a bad idea: reader-writer
     * mutexes cause cache-line ping-ponging when there is contention,
     * effecitvely serializing access, and are just plain slower when
     * there is no contention.  Thus, the current implementations seem
     * to be a lose-lose proposition. See the Anthony Williams post here:
     * http://permalink.gmane.org/gmane.comp.lib.boost.devel/211180
     */
    mutable std::mutex type_mutex;

    /* Only one module can add types at a time. */
    mutable std::mutex _module_mutex;
    mutable int _tmod;

    Type nTypes;
    Type nValues;
    Type _maxDepth;

    std::vector< std::vector<bool> > inheritanceMap;
    std::vector< std::vector<bool> > recursiveMap;
    std::unordered_map<std::string, Type> name2CodeMap;
    std::vector<const std::string*> _code2NameMap;
    std::vector<const std::string*> _code2ShortMap;
    std::vector<int> _mod;
    std::vector<size_t> _hash;
    TypeSignal _addTypeSignal;

    void setParentRecursively(Type parent, Type type, Type& maxd);

public:
    /** Gets the singleton instance (following meyer's design pattern) */
    friend NameServer& nameserver();

    bool beginTypeDecls(const char *);
    void endTypeDecls(void);
    /**
     * Adds a new atom type with the given name and parent type.
     * Return a numeric value that is assigned to the new type.
     */
    Type declType(const Type parent,
                  const std::string& name,
                  const std::string& shrt = "");

    /** Provides ability to get type-added signals.
     * @warning methods connected to this signal must not call
     * ClassServer::addType or things will deadlock.
     */
    TypeSignal& typeAddedSignal();

    /**
     * Given the type `type`, get the immediate descendents.
     * This is NOT recursive, only the first generation is returned.
     * Stores the children types on the OutputIterator 'result'.
     * Returns the number of children types.
     */
    template<typename OutputIterator>
    unsigned long getChildren(Type type, OutputIterator result) const
    {
        unsigned long n_children = 0;
        for (Type i = type+1; i < nTypes; ++i) {
            if (inheritanceMap[type][i]) {
                *(result++) = i;
                n_children++;
            }
        }
        return n_children;
    }

    /**
     * Given the type `type`, get the immediate parents.
     * This is NOT recursive, only the first generation is returned.
     * Stores the parent types on the OutputIterator 'result'.
     * Returns the number of parent types.
     */
    template<typename OutputIterator>
    unsigned long getParents(Type type, OutputIterator result) const
    {
        unsigned long n_parents = 0;
        for (Type i = 0; i < type; ++i) {
            if (inheritanceMap[i][type]) {
                *(result++) = i;
                n_parents++;
            }
        }
        return n_parents;
    }

    /**
     * Given the type `type`, get all of the descendents.  This is
     * recursive, that is, children of the children are returned.
     * Stores the children types on the OutputIterator 'result'.
     * Returns the number of children types.
     */
    template <typename OutputIterator>
    unsigned long getChildrenRecursive(Type type, OutputIterator result) const
    {
        unsigned long n_children = 0;
        for (Type i = type+1; i < nTypes; ++i) {
            if (recursiveMap[type][i]) {
                *(result++) = i;
                n_children++;
            }
        }
        return n_children;
    }
    TypeSet getChildrenRecursive(Type type) const
    {
        TypeSet ts;
        for (Type i = type+1; i < nTypes; ++i) {
            if (recursiveMap[type][i]) {
                ts.insert(i);
            }
        }
        return ts;
    }

    /**
     * Given the type `type`, get all of the parents. This is
     * recursive, that is, parents of the parents are returned.
     * Stores the parent types on the OutputIterator 'result'.
     * Returns the number of parent types.
     */
    template <typename OutputIterator>
    unsigned long getParentsRecursive(Type type, OutputIterator result) const
    {
        unsigned long n_parents = 0;
        for (Type i = 0; i < type; ++i) {
            if (recursiveMap[i][type]) {
                *(result++) = i;
                n_parents++;
            }
        }
        return n_parents;
    }
    TypeSet getParentsRecursive(Type type) const
    {
        TypeSet ts;
        for (Type i = 0; i < type; ++i) {
            if (recursiveMap[i][type]) {
                ts.insert(i);
            }
        }
        return ts;
    }

    template <typename Function>
    void foreachRecursive(Function func, Type type) const
    {
        for (Type i = 0; i < nTypes; ++i) {
            if (recursiveMap[type][i]) (func)(i);
        }
    }

    /**
     * Returns the total number of classes in the system.
     *
     * @return The total number of classes in the system.
     */
    Type getNumberOfClasses() const { return nTypes; }

    /**
     * Returns whether a given class is assignable from another.
     * This is the single-most commonly called method in this class.
     *
     * @param super Super class.
     * @param sub Subclass.
     * @return Whether a given class is assignable from another.
     */
    bool isA(Type sub, Type super) const
    {
        /* Because this method is called extremely often, we want
         * the best-case fast-path for it.  Since updates are extremely
         * unlikely after initialization, we use a multi-reader lock,
         * and don't care at all about writer starvation, since there
         * will almost never be writers. However, see comments above
         * about multi-reader-locks -- we are not using them just right
         * now, because they don't seem to actually help.
         *
         * Currently, this lock accounts for 2% or 3% performance
         * impact on atom insertion into atomspace.  The unit tests
         * don't need it to pass.  Most users probably dont need it
         * at all, because most type creation/update happens in
         * shared-lib ctors, which mostly should be done by the time
         * that this gets called. How big a price do you want to pay
         * for avoiding a possible crash on a shared-lib load while
         * also running some multi-threaded app?
         */
        // std::lock_guard<std::mutex> l(type_mutex);
        if ((sub >= nTypes) || (super >= nTypes)) return false;
        return recursiveMap[super][sub];
    }

    bool isAncestor(Type super, Type sub) const;

    /**
     * Returns true if given class is a Value.
     *
     * @param t class.
     * @return Whether a given class is Value.
     */
    bool isValue(Type t) const { return isA(t, VALUE); }

    /**
     * Returns true if given class is a valid atom type.
     *
     * @param t class.
     * @return Whether a given class is an atom.
     */
    bool isAtom(Type t) const { return isA(t, ATOM); }

    /**
     * Returns true if given class is a Node.
     *
     * @param t class.
     * @return Whether a given class is Node.
     */
    bool isNode(Type t) const { return isA(t, NODE); }

    /**
     * Returns true if given class is a Link.
     *
     * @param t class.
     * @return Whether a given class is Link.
     */
    bool isLink(Type t) const { return isA(t, LINK); }

    /**
     * Returns whether a class with name 'typeName' is defined.
     */
    bool isDefined(const std::string& typeName) const;
    bool isDefined(Type) const;

    /**
     * Returns the type of a given class.
     *
     * @param typeName Class type name.
     * @return The type of a givenn class.
     */
    Type getType(const std::string& typeName) const;

    /**
     * Returns the string representation of a given atom type.
     *
     * @param type Atom type code.
     * @return The string representation of a given class.
     */
    const std::string& getTypeName(Type type) const;
    const std::string& getTypeShortName(Type type) const;

    /**
     * Returns a hash for the atom type. The hash does not depend on
     * the type number, and thus remains stable as types are added
     * and removed from the type hierarchy. The hash is computed from
     * the string name of the type.
     *
     * @param type Atom type code.
     * @return A corresponding hash.
     */
    size_t getTypeHash(Type type) const { return _hash[type]; }
};

NameServer& nameserver();


/** @}*/
} // namespace opencog

#endif // _OPENCOG_CLASS_NAMESERVER_H
