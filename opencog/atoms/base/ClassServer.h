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
#include <unordered_map>
#include <vector>

#include <boost/signals2.hpp>

#include <opencog/atoms/base/types.h>
#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/Handle.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

typedef boost::signals2::signal<void (Type)> TypeSignal;

class ProtoAtom;
typedef std::shared_ptr<ProtoAtom> ProtoAtomPtr;

/**
 * This class keeps track of the complete protoatom (value and atom)
 * class hierarchy. It also provides factories for those atom types
 * that have non-trivial C++ objects behind them.
 */
class ClassServer
{
public:
    // Currently, we provide factories only for atoms, not for
    // values.
    typedef Handle (AtomFactory)(const ProtoAtomPtr&);
private:

    /** Private default constructor for this class to make it a singleton. */
    ClassServer();

    /* It is very tempting to make the type_mutex into a reader-writer
     * mutex. However, it appears that this is a bad idea: reader-writer
     * mutexes cause cache-line ping-ponging when there is contention,
     * effecitvely serializing access, and are just plain slower when
     * there is no contention.  Thus, the current implementations seem
     * to be a lose-lose proposition. See the Anthony Williams post here:
     * http://permalink.gmane.org/gmane.comp.lib.boost.devel/211180
     */
    mutable std::mutex type_mutex;

    Type nTypes;

    std::vector< std::vector<bool> > inheritanceMap;
    std::vector< std::vector<bool> > recursiveMap;
    std::unordered_map<std::string, Type> name2CodeMap;
    std::unordered_map<Type, const std::string*> code2NameMap;
    std::unordered_map<Type, AtomFactory*> _atomFactory;
    TypeSignal _addTypeSignal;

    void setParentRecursively(Type parent, Type type);

public:
    /** Gets the singleton instance (following meyer's design pattern) */
    friend ClassServer& classserver();

    /**
     * Adds a new atom type with the given name and parent type.
     * Return a numeric value that is assigned to the new type.
     */
    Type addType(const Type parent, const std::string& name);

    /**
     * Declare a factory for an atom type.
     */
    void addFactory(Type, AtomFactory*);

    /**
     * Convert the indicated ProtoAtom into a C++ instance of the
     * same type.
     */
    Handle factory(const ProtoAtomPtr&);

    /** Provides ability to get type-added signals.
     * @warning methods connected to this signal must not call ClassServer::addType or
     * things will deadlock.
     */
    TypeSignal& addTypeSignal();

    /**
     * Stores the children types on the OutputIterator 'result'. Returns the
     * number of children types.
     */
    template<typename OutputIterator>
    unsigned long getChildren(Type type, OutputIterator result)
    {
        unsigned long n_children = 0;
        for (Type i = 0; i < nTypes; ++i) {
            if (inheritanceMap[type][i] && (type != i)) {
                *(result++) = i;
                n_children++;
            }
        }
        return n_children;
    }

    template <typename OutputIterator>
    unsigned long getChildrenRecursive(Type type, OutputIterator result)
    {
        unsigned long n_children = 0;
        for (Type i = 0; i < nTypes; ++i) {
            if (recursiveMap[type][i] && (type != i)) {
                *(result++) = i;
                n_children++;
            }
        }
        return n_children;
    }

    template <typename Function>
    void foreachRecursive(Function func, Type type)
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
    Type getNumberOfClasses();

    /**
     * Returns whether a given class is assignable from another.
     * This is the single-most commonly called method in this class.
     *
     * @param super Super class.
     * @param sub Subclass.
     * @return Whether a given class is assignable from another.
     */
    bool isA(Type sub, Type super)
    {
        /* Because this method is called extremely often, we want
         * the best-case fast-path for it.  Since updates are extremely
         * unlikely after initialization, we use a multi-reader lock,
         * and don't care at all about writer starvation, since there
         * will almost never be writers. However, see comments above
         * about multi-reader-locks -- we are not using them just right
         * now, because they don't seem to actually help. */
        std::lock_guard<std::mutex> l(type_mutex);
        if ((sub >= nTypes) || (super >= nTypes)) return false;
        return recursiveMap[super][sub];
    }

    bool isA_non_recursive(Type sub, Type super);

    /**
     * Returns true if given class is a Value.
     *
     * @param t class.
     * @return Whether a given class is Value.
     */
    bool isValue(Type t) { return isA(t, VALUE); }

    /**
     * Returns true if given class is a valid atom type.
     *
     * @param t class.
     * @return Whether a given class is an atom.
     */
    bool isAtom(Type t) { return isA(t, ATOM); }

    /**
     * Returns true if given class is a Node.
     *
     * @param t class.
     * @return Whether a given class is Node.
     */
    bool isNode(Type t) { return isA(t, NODE); }

    /**
     * Returns true if given class is a Link.
     *
     * @param t class.
     * @return Whether a given class is Link.
     */
    bool isLink(Type t) { return isA(t, LINK); }

    /**
     * Returns whether a class with name 'typeName' is defined.
     */
    bool isDefined(const std::string& typeName);

    /**
     * Returns the type of a given class.
     *
     * @param typeName Class type name.
     * @return The type of a givenn class.
     */
    Type getType(const std::string& typeName);

    /**
     * Returns the string representation of a given atom type.
     *
     * @param type Atom type code.
     * @return The string representation of a givenn class.
     */
    const std::string& getTypeName(Type type);
};

ClassServer& classserver();

/** @}*/
} // namespace opencog

#endif // _OPENCOG_CLASS_SERVER_H
