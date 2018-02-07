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
#include <opencog/util/Logger.h>

#include <opencog/atoms/base/types.h>
#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/ProtoAtom.h>

//#define DPRINTF printf
#define DPRINTF(...)

using namespace opencog;

ClassServer::ClassServer(void)
{
	nTypes = 0;
	_maxDepth = 0;
}

static int tmod = 0;

/**
 * Return true if this module has already been loaded.
 *
 * This is a strange kind of hack to allow the cogserver unit
 * tests to be run from the build directory, while *also* having
 * libraries that those unit tests require be installed in the
 * system directories.
 *
 * Essentially, this is a manual implementation of the so-called
 * ODR (One Definition Rule) that the linker-loader uses to avoid
 * loading the same library twice, and extending it so that it works
 * when the "same library" is available in the build directory and
 * in some system directory. These are not actually the "same library"
 * from the point of view of the linker-loader; but they need to be,
 * to get the unit tests to pass.
 *
 * The primary issue is driven by mixed python+scheme unit tests.
 * They will typically load libraries in the build dir, and then,
 * because they use modules, try to load the same lirbries again,
 * this time from the system dir.  This results in the same atom
 * types getting defined twice, and then things go downhill.
 */
bool ClassServer::beginTypeDecls(const char * mod_name)
{
   std::lock_guard<std::mutex> l(type_mutex);
	std::string mname(mod_name);
	if (_loaded_modules.end() != _loaded_modules.find(mname))
		return true;

	_loaded_modules.insert(mname);
	tmod++;
	return false;
}

void ClassServer::endTypeDecls(void)
{
	// Valid types are odd-numbered.
	tmod++;
}

Type ClassServer::declType(const Type parent, const std::string& name)
{
    if (1 != tmod%2)
        throw InvalidParamException(TRACE_INFO,
            "Improper type declararition; must call beginTypeDecls() first\n");

    // Check if a type with this name already exists. If it does, then
    // the second and subsequent calls are to be interpreted as defining
    // multiple inheritance for this type.  A real-life example is the
    // GroundedSchemeNode, which inherits from several types.
    Type type = getType(name);
    if (type != NOTYPE) {
        std::lock_guard<std::mutex> l(type_mutex);

        // ... unless someone is accidentally declaring the same type
        // in some different place. In that case, we throw an error.
        if (_mod[type] != tmod)
            throw InvalidParamException(TRACE_INFO,
                "Type \"%s\" has already been declared (%d)\n",
                name.c_str(), type);

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
    _code2NameMap.resize(nTypes);
    _mod.resize(nTypes);
    _atomFactory.resize(nTypes);
    _validator.resize(nTypes);

    for (auto& bv: inheritanceMap) bv.resize(nTypes, false);
    for (auto& bv: recursiveMap) bv.resize(nTypes, false);

    inheritanceMap[type][type]   = true;
    inheritanceMap[parent][type] = true;
    recursiveMap[type][type]     = true;
    name2CodeMap[name]           = type;
    _code2NameMap[type]          = &(name2CodeMap.find(name)->first);
    _mod[type]                   = tmod;

    Type maxd = 1;
    setParentRecursively(parent, type, maxd);
    if (_maxDepth < maxd) _maxDepth = maxd;

    // unlock mutex before sending signal which could call
    l.unlock();

    // Emit add type signal.
    _addTypeSignal.emit(type);

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

TypeSignal& ClassServer::typeAddedSignal()
{
    return _addTypeSignal;
}

static Handle validating_factory(const Handle& atom_to_check)
{
	ClassServer::Validator* checker =
		classserver().getValidator(atom_to_check->get_type());

	/* Well, is it OK, or not? */
	if (not checker(atom_to_check))
		throw SyntaxException(TRACE_INFO,
		     "Invalid Atom syntax: %s",
		     atom_to_check->to_string().c_str());

	return atom_to_check;
}

void ClassServer::spliceFactory(Type t, AtomFactory* fact)
{
	// Find all the factories that belong to parents of this type.
	std::set<AtomFactory*> ok_to_clobber;
	ok_to_clobber.insert(validating_factory);
	for (Type parent=0; parent < t; parent++)
	{
		if (recursiveMap[parent][t] and _atomFactory[parent])
			ok_to_clobber.insert(_atomFactory[parent]);
	}

	// Set the factory for all children of this type.  Be careful
	// not to clobber any factories that might have been previously
	// declared.
	for (Type chi=t; chi < nTypes; chi++)
	{
		if (recursiveMap[t][chi] and
		    (nullptr == _atomFactory[chi] or
		    ok_to_clobber.end() != ok_to_clobber.find(_atomFactory[chi])))
		{
			_atomFactory[chi] = fact;
		}
	}
}

void ClassServer::addFactory(Type t, AtomFactory* fact)
{
	std::unique_lock<std::mutex> l(type_mutex);
	spliceFactory(t, fact);
}

void ClassServer::addValidator(Type t, Validator* checker)
{
	std::unique_lock<std::mutex> l(type_mutex);
	_validator[t] = checker;
	spliceFactory(t, validating_factory);

	for (Type chi=t; chi < nTypes; chi++)
	{
		if (recursiveMap[t][chi])
			_validator[chi] = checker;
	}
}

ClassServer::AtomFactory* ClassServer::getFactory(Type t) const
{
	return _atomFactory[t];
}

ClassServer::Validator* ClassServer::getValidator(Type t) const
{
	return _validator[t];
}

Handle ClassServer::factory(const Handle& h) const
{
	// If there is a factory, then use it.
	AtomFactory* fact = getFactory(h->get_type());
	if (fact)
		return (*fact)(h);

	return h;
}

Type ClassServer::getNumberOfClasses() const
{
    return nTypes;
}

bool ClassServer::isA_non_recursive(Type type, Type parent) const
{
    std::lock_guard<std::mutex> l(type_mutex);
    if ((type >= nTypes) || (parent >= nTypes)) return false;
    return inheritanceMap[parent][type];
}

bool ClassServer::isDefined(const std::string& typeName) const
{
    std::lock_guard<std::mutex> l(type_mutex);
    return name2CodeMap.find(typeName) != name2CodeMap.end();
}

Type ClassServer::getType(const std::string& typeName) const
{
    std::lock_guard<std::mutex> l(type_mutex);
    std::unordered_map<std::string, Type>::const_iterator it = name2CodeMap.find(typeName);
    if (it == name2CodeMap.end()) {
        return NOTYPE;
    }
    return it->second;
}

const std::string& ClassServer::getTypeName(Type type) const
{
    static std::string nullString = "*** Unknown Type! ***";

    if (nTypes <= type) return nullString;

    std::lock_guard<std::mutex> l(type_mutex);
    const std::string* name = _code2NameMap[type];
    if (name) return *name;
    return nullString;
}

ClassServer& opencog::classserver()
{
    static std::unique_ptr<ClassServer> instance(new ClassServer());
    return *instance;
}
