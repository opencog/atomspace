/*
 * opencog/atoms/value/ValueFactory.h
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
#ifndef _VALUE_FACTORY_H_
#define _VALUE_FACTORY_H_

#include "Value.h"
#include <opencog/util/exceptions.h>

#include <map>
#include <typeinfo>
#include <typeindex>
#include <vector>

namespace opencog
{
using ValueFactory = ValuePtr (*) (...);
using ValueCaster = ValuePtr (*) (const ValuePtr&);

template<typename... Types>
static std::vector<std::type_index> to_list_of_type_indexes()
{
	return std::vector<std::type_index>{ std::type_index(typeid(Types))... };
}

class ValueServer
{
    friend ValueServer& valueserver();
private:
    ValueServer() {}
    static std::string demangle(std::type_index);

    struct ProtoFactory
    {
        ValueFactory func;
        std::vector<std::type_index> args;
    };

    std::map<Type, std::vector<ProtoFactory>> _factories;
    std::map<Type, ValueCaster> _vcasters;

public:

    /**
     * Registers the creator functions.
     *
     * @param vtype The value type.
     * @param func  a pointer to the creator function of the value.
     * @param args  an ordered vector of the types of arguments the
     *              creator takes.
     */
    void addFactory(Type vtype, ValueFactory func,
                    std::vector<std::type_index> args);

     /**
      * Registers the casting function for a given type.
      *
      * @param vtype  the value type.
      * @param caster the casting function.
      */
    void addCaster(Type vtype, ValueCaster caster);

     /**
      * Casts a ValuePtr object into its type's Value pointer.
      *
      * @param ptr the ValuePtr to be cast.
      */
    ValuePtr recast(const ValuePtr& ptr) const;

     /**
      * Does dynamic dispatching of the appropriate create functions
      * which matches argument provided.
      * @param arg the value type constructor argument.
      * @param vtype The type of the Value.
      * @throws invalid_argument exception.
      */
    template <typename TYP, typename... ARG>
    ValuePtr create(TYP vtype, ARG&&... arg) const
    {
        // Look up the factory only once; cache the result.
        // There is one distinct copy of `fax` for each
        // template ARG. However, TYP is always a short,
        // and we cannot know what vtype is at compile time.
        // So we have to do one run-time lookup, in a vector.
        static std::vector<ValueFactory> fax;
        static std::mutex mtx;

        ValueFactory fptr = nullptr;
        try
        {
            fptr = fax.at(vtype);
        }
        catch(...) {}

        if (nullptr == fptr)
        {
            try
            {
                // First, find the list of factories for this type.
                std::vector<ProtoFactory> func_vec = _factories.at(vtype);

                // Second, get a list of types expected
                std::vector<std::type_index> expected_args =
                    to_list_of_type_indexes<ARG...>();

                // Third, find the matching arglist.
                for (const ProtoFactory& fr : func_vec)
                {
                    if (fr.args == expected_args)
                    {
                        fptr = fr.func;

                        std::lock_guard<std::mutex> lck(mtx);
                        std::vector<ValueFactory> newfax(fax);
                        if (newfax.size() <= vtype)
                            newfax.resize(vtype+1);
                        newfax[vtype] = fr.func;
                        // swap here allows lookup to be lockless.
                        fax.swap(newfax);
                        break;
                    }
                }
            }
            catch(...) {}
        }

        if (fptr)
            return (*fptr)(&arg...);

        std::vector<std::type_index> expected_args =
                        to_list_of_type_indexes<ARG...>();
        std::string argnames;
        for (auto t : expected_args)
            argnames += demangle(t) + " ";

        throw IndexErrorException(TRACE_INFO,
            "No factory found for Value type %d - %s(%s)",
            vtype, nameserver().getTypeName(vtype).c_str(),
            argnames.c_str());
    }
};

ValueServer& valueserver();

#define TOKENPASTE(x, y) x ## y
#define TOKENPASTE2(x, y) TOKENPASTE(x, y)
#define DEFINE_VALUE_FACTORY(CTYPE,CREATE,...)                       \
                                                                     \
/* This runs when the shared lib is loaded. */                       \
/* Set priority to 110 so that it runs after nameserver. */          \
static __attribute__ ((constructor (110))) void                      \
    TOKENPASTE2(init, __COUNTER__)(void)                             \
{                                                                           \
   valueserver().addFactory(CTYPE, (ValueFactory) & (CREATE<__VA_ARGS__>),  \
      to_list_of_type_indexes<__VA_ARGS__>());                              \
}

}
#endif
