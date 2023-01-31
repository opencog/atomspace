/*
 * opencog/atoms/value/NameServer.cc
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

#include "NameServer.h"

#include <exception>

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/value/Value.h>
#include <opencog/util/exceptions.h>


// Maximum number of values that can be defined without error.
#define MAX_NUM_VALUE 64

using namespace opencog;

NameServer::NameServer(void)
{
	nTypes = MAX_NUM_VALUE + 1;
	nValues = 0;   // TopType is 0  Value is 1
	_maxDepth = 0;
	_tmod = 0;
}

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
bool NameServer::beginTypeDecls(const char * mod_name)
{
	// Prevent anyone else from performing type decls.
	_module_mutex.lock();

	std::string mname(mod_name);
	if (_loaded_modules.end() != _loaded_modules.find(mname))
	{
		_module_mutex.unlock();
		return true;
	}

	_tmod++;
	_loaded_modules.insert(mname);
	return false;
}

void NameServer::endTypeDecls(void)
{
	// Valid types are odd-numbered.
	_tmod++;
	_module_mutex.unlock();

	classserver().update_factories();
}

Type NameServer::declType(const Type parent,
                          const std::string& name,
                          const std::string& short_name)
{
    if (1 != _tmod%2)
        throw InvalidParamException(TRACE_INFO,
            "Improper type declararition; must call beginTypeDecls() first\n");

    // Check if a type with this name already exists. If it does, then
    // the second and subsequent calls are to be interpreted as defining
    // multiple inheritance for this type.  A real-life example is the
    // GroundedSchemaNode, which inherits from several types.
    Type type = getType(name);
    if (type != NOTYPE) {
        std::lock_guard<std::mutex> l(type_mutex);

        // ... unless someone is accidentally declaring the same type
        // in some different place. In that case, we throw an error.
        if (_mod[type] != _tmod)
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
    if (0 == ATOM or parent < ATOM)
    {
        if (0 == ATOM and 0 == name.compare("Atom"))
        {
            type = MAX_NUM_VALUE;
        }
        else
        {
            if (MAX_NUM_VALUE <= nValues)
                throw InvalidParamException(TRACE_INFO,
                    "Maximum number %d of Value declarations exceeded!\n"
                    "Increase MAX_NUM_VALUE in NameServer.cc and recompile!\n",
                    MAX_NUM_VALUE);
            type = nValues++;
        }
    }
    else
    {
        type = nTypes++;
    }

    // Resize inheritanceMap container.
    inheritanceMap.resize(nTypes);
    recursiveMap.resize(nTypes);
    _code2NameMap.resize(nTypes);
    _code2ShortMap.resize(nTypes);
    _mod.resize(nTypes);
    _hash.resize(nTypes);

    for (auto& bv: inheritanceMap) bv.resize(nTypes, false);
    for (auto& bv: recursiveMap) bv.resize(nTypes, false);

    inheritanceMap[type][type]   = true;
    inheritanceMap[parent][type] = true;
    recursiveMap[type][type]     = true;
    name2CodeMap[name]           = type;
    _code2NameMap[type]          = &(name2CodeMap.find(name)->first);
    _mod[type]                   = _tmod;
    _hash[type]                  = std::hash<std::string>{}(name);

    Type maxd = 1;
    setParentRecursively(parent, type, maxd);
    if (_maxDepth < maxd) _maxDepth = maxd;

    // Short-hand names ... without the trailing "Node", "Link" at the
    // end. Must be explicitly declared by the caller, else defaults
    // to the long name.
    if (0 < short_name.size())
    {
        name2CodeMap[short_name] = type;
       _code2ShortMap[type]      = &(name2CodeMap.find(short_name)->first);
    }
    else
    {
       _code2ShortMap[type]      = &(name2CodeMap.find(name)->first);
    }

    // unlock mutex before sending signal which could call
    l.unlock();

    // Emit add type signal.
    _addTypeSignal.emit(type);

    return type;
}

void NameServer::setParentRecursively(Type parent, Type type, Type& maxd)
{
    if (recursiveMap[parent][type]) return;

    if (type <= parent)
        throw InvalidParamException(TRACE_INFO,
            "Improper type declararition; "
            "parent (%d) must be smaller than child (%d).\n"
            "(Partial orders must be strict!)\n", parent, type);

    bool incr = false;
    recursiveMap[parent][type] = true;
    for (Type i = 0; i < parent; ++i) {
        if (recursiveMap[i][parent]) {
            incr = true;
            setParentRecursively(i, type, maxd);
        }
    }
    if (incr) maxd++;
}

TypeSignal& NameServer::typeAddedSignal()
{
    return _addTypeSignal;
}

bool NameServer::isAncestor(Type super, Type sub) const
{
	std::lock_guard<std::mutex> l(type_mutex);
	return recursiveMap[super][sub];
}

bool NameServer::isDefined(const std::string& typeName) const
{
    std::lock_guard<std::mutex> l(type_mutex);
    return name2CodeMap.find(typeName) != name2CodeMap.end();
}

bool NameServer::isDefined(Type t) const
{
    std::lock_guard<std::mutex> l(type_mutex);
    return (1 <= t and t < nValues) or (ATOM <= t and t < nTypes);
}

Type NameServer::getType(const std::string& typeName) const
{
    std::lock_guard<std::mutex> l(type_mutex);
    std::unordered_map<std::string, Type>::const_iterator it = name2CodeMap.find(typeName);
    if (it == name2CodeMap.end()) {
        return NOTYPE;
    }
    return it->second;
}

const std::string& NameServer::getTypeName(Type type) const
{
    static std::string nullString = "*** Unknown Type! ***";
    static std::string bottomString = "*** Bottom Type! ***";

    if (NOTYPE == type) return bottomString;
    if (nTypes <= type) return nullString;

    std::lock_guard<std::mutex> l(type_mutex);
    const std::string* name = _code2NameMap[type];
    if (name) return *name;
    return nullString;
}

const std::string& NameServer::getTypeShortName(Type type) const
{
    static std::string nullString = "*** Unknown Type! ***";
    static std::string bottomString = "*** Bottom Type! ***";

    if (NOTYPE == type) return bottomString;
    if (nTypes <= type) return nullString;

    std::lock_guard<std::mutex> l(type_mutex);
    const std::string* name = _code2ShortMap[type];
    if (name) return *name;
    return nullString;
}

NameServer& opencog::nameserver()
{
    static std::unique_ptr<NameServer> instance(new NameServer());
    return *instance;
}
