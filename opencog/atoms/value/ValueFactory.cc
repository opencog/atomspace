/*
 * opencog/atoms/value/ValueFactory.cc
 *
 * Copyright (C) 2015,2025 Linas Vepstas
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

#include <cxxabi.h> // for abi::__cxa_demangle
#include "ValueFactory.h"

using namespace opencog;

/// Convert c++ encoded name e.g. N7opencog6HandleE to pretty name
/// e.g. opencog::Handle in the above example.
std::string ValueServer::demangle(std::type_index ti)
{
	int status(0);
	char *dname = abi::__cxa_demangle(ti.name(), nullptr, nullptr, &status);
	std::string rv(dname);
	free(dname);
	return rv;
}

void ValueServer::addFactory(Type vtype, ValueFactory func,
                             std::vector<std::type_index> args)
{
    ProtoFactory fr = {func, args};

    if (_factories.find(vtype) != _factories.end())
        _factories[vtype].push_back(fr);
    else
        _factories[vtype] = {fr};

    // Annoyingly-annoying special case for VoidValue.
    if (0 == args.size() or args[0] == std::type_index(typeid(void)))
    {
       ProtoFactory fr = {func, std::vector<std::type_index>()};
       _factories[vtype] = {fr};
    }
}

void ValueServer::addCaster(Type vtype, ValueCaster func)
{
    _vcasters[vtype] = func;
}

ValuePtr ValueServer::recast(const ValuePtr& ptr) const
{
    Type vtype = ptr->get_type();
    try
    {
        ValueCaster caster = _vcasters.at(vtype);
        return (*caster)(ptr);
    }
    catch (...)
    {
        return ptr;
    }
}

ValueServer& opencog::valueserver()
{
    static std::unique_ptr<ValueServer> instance(new ValueServer());
    return *instance;
}
