/*
 * opencog/atoms/base/ClassServer.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2008 by OpenCog Foundation
 * Copyright (C) 2009,2017 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
 *
 * Written by Thiago Maia <thiago@vettatech.com>
 *            Andre Senna <senna@vettalabs.com>
 *            Gustavo Gama <gama@vettalabs.com>
 *            Linas Vepstas <linasvepstas@gmail.com>
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

#include "ClassServer.h"

#include <exception>

#include <opencog/atoms/base/types.h>
#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/ProtoAtom.h>

#include "opencog/atoms/base/atom_types.definitions"

//#define DPRINTF printf
#define DPRINTF(...)

using namespace opencog;

ClassServer::ClassServer(void)
{
    nTypes = 0;
    _maxDepth = 0;
}

Type ClassServer::addType(const Type parent, const std::string& name)
{
    // Check if a type with this name already exists. If it does, then
    // the second and subsequent calls are to be interpreted as defining
    // multiple inheritance for this type.  A real-life example is the
    // GroundedSchemeNode, which inherits from several types.
    Type type = getType(name);
    if (type != NOTYPE) {
        std::lock_guard<std::mutex> l(type_mutex);
        DPRINTF("Type \"%s\" has already been added (%d)\n", name.c_str(), type);
        inheritanceMap[parent][type] = true;

        Type maxd = 1;
        setParentRecursively(parent, type, maxd);
        if (_maxDepth < maxd) _maxDepth = maxd;
        return type;
    }

    std::unique_lock<std::mutex> l(type_mutex);
    // Assign type code and increment type counter.
    type = nTypes++;

    // Resize inheritanceMap container.
    inheritanceMap.resize(nTypes);
    recursiveMap.resize(nTypes);

    for (auto& bv: inheritanceMap) bv.resize(nTypes, false);
    for (auto& bv: recursiveMap) bv.resize(nTypes, false);

    inheritanceMap[type][type]   = true;
    inheritanceMap[parent][type] = true;
    recursiveMap[type][type]     = true;
    name2CodeMap[name]           = type;
    code2NameMap[type]           = &(name2CodeMap.find(name)->first);

    Type maxd = 1;
    setParentRecursively(parent, type, maxd);
    if (_maxDepth < maxd) _maxDepth = maxd;

    // unlock mutex before sending signal which could call
    l.unlock();

    // Emit add type signal.
    _addTypeSignal(type);

    return type;
}

void ClassServer::setParentRecursively(Type parent, Type type, Type& maxd)
{
    if (recursiveMap[parent][type]) return;

    bool incr = false;
    recursiveMap[parent][type] = true;
    for (Type i = 0; i < nTypes; ++i) {
        if ((recursiveMap[i][parent]) and (i != parent)) {
            incr = true;
            setParentRecursively(i, type, maxd);
        }
    }
    if (incr) maxd++;
}

boost::signals2::signal<void (Type)>& ClassServer::addTypeSignal()
{
    return _addTypeSignal;
}

void ClassServer::addFactory(Type t, AtomFactory* fact)
{
    std::unique_lock<std::mutex> l(type_mutex);
    _atomFactory[t] = fact;
}

// Perform a depth-first recursive search for a factory,
// up to a maximum depth.
ClassServer::AtomFactory* ClassServer::searchToDepth(Type t, int depth)
{
	// If there is a factory, then return it.
	auto fpr = _atomFactory.find(t);
	if (_atomFactory.end() != fpr)
		return fpr->second;

	// Perhaps one of the parent types has a factory.
	// Perform a depth-first recursion.
	depth--;
	if (depth < 0) return nullptr;

	std::vector<Type> parents;
	getParents(t, back_inserter(parents));
	for (auto p: parents)
	{
		AtomFactory* fact = searchToDepth(p, depth);
		if (fact) return fact;
	}

	return nullptr;
}

ClassServer::AtomFactory* ClassServer::getFactory(Type t)
{
	// If there is a factory, then return it.
	auto fpr = _atomFactory.find(t);
	if (_atomFactory.end() != fpr)
		return fpr->second;

	// Perhaps one of the parent types has a factory.
	//
	// We want to use a breadth-first recursion, and not
	// the simpler-to-code depth-first recursion.  That is,
	// we do NOT want some deep parent factory, when there
	// is some factory at a shallower level.
	//
	for (int search_depth = 1; search_depth <= _maxDepth; search_depth++)
	{
		AtomFactory* fact = searchToDepth(t, search_depth);
		if (fact) return fact;
	}

	return nullptr;
}

Handle ClassServer::factory(const Handle& h)
{
	// If there is a factory, then use it.
	AtomFactory* fact = getFactory(h->getType());
	if (fact)
		return (*fact)(h);

	return h;
}

Type ClassServer::getNumberOfClasses()
{
    return nTypes;
}

bool ClassServer::isA_non_recursive(Type type, Type parent)
{
    std::lock_guard<std::mutex> l(type_mutex);
    if ((type >= nTypes) || (parent >= nTypes)) return false;
    return inheritanceMap[parent][type];
}

bool ClassServer::isDefined(const std::string& typeName)
{
    std::lock_guard<std::mutex> l(type_mutex);
    return name2CodeMap.find(typeName) != name2CodeMap.end();
}

Type ClassServer::getType(const std::string& typeName)
{
    std::lock_guard<std::mutex> l(type_mutex);
    std::unordered_map<std::string, Type>::iterator it = name2CodeMap.find(typeName);
    if (it == name2CodeMap.end()) {
        return NOTYPE;
    }
    return it->second;
}

const std::string& ClassServer::getTypeName(Type type)
{
    static std::string nullString = "*** Unknown Type! ***";

    std::lock_guard<std::mutex> l(type_mutex);
    std::unordered_map<Type, const std::string*>::iterator it;
    if ((it = code2NameMap.find(type)) != code2NameMap.end())
        return *(it->second);
    return nullString;
}

ClassServer& opencog::classserver()
{
    static std::unique_ptr<ClassServer> instance(new ClassServer());
    return *instance;
}

// This runs when the shared lib is loaded.  We have to make
// sure that all of the core types are initialized before
// anything else happens, as otherwise weird symptoms manifest.
static __attribute__ ((constructor)) void init(void)
{
	classserver();

	// Autogenerated code to initialize all atom types is defined
	// the in atom_types.script file.
	#include "opencog/atoms/base/atom_types.inheritance"
}
