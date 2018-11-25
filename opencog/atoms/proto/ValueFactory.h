#ifndef _VALUE_FACTORY_H_
#define _VALUE_FACTORY_H_

#include "ProtoAtom.h"
#include <opencog/atoms/base/Handle.h>
#include <opencog/util/exceptions.h>

#include <map>
#include <typeinfo>
#include <typeindex>
#include <vector>

namespace opencog
{
using CreateProto = ProtoAtomPtr (*) (...);
using ValueCaster = ProtoAtomPtr (*) (const ProtoAtomPtr&);


class ValueServer
{
    friend ValueServer& valueserver();
private:
    ValueServer() {}
    
    struct ProtoFactory
    {
        CreateProto func;
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
    void addFactory(Type vtype, CreateProto func,
                    std::vector<std::type_index> args);

     /**
      * Registers the casting function for a given type.
      *
      * @param vtype  the value type.
      * @param caster the casting function.
      */
    void addCaster(Type vtype, ValueCaster caster);

     /**
      * Casts a protoAtomPtr object into its type's Value pointer.
      *
      * @param ptr the protoAtomPtr to be cast.
      */
    ProtoAtomPtr recast(ProtoAtomPtr ptr);

     /**
      * Does dynamic dispatching of the appropriate create functions
      * which matches argument provided.
      * @param arg the value type constructor argument.
      * @param vtype The type of the Value.
      * @throws invalid_argument exception.
      */
    template <typename T>
    ProtoAtomPtr create(Type vtype, T arg)
    {
        // Once we know there is a matching function, cache.
        static std::map<Type, CreateProto> cache = {};
        CreateProto  fptr = nullptr;

        if (cache.find(vtype) != cache.end())
        {
            fptr = cache[vtype];
        }
        else
        {
            if (_factories.find(vtype) != _factories.end())
            {
                std::vector<ProtoFactory> func_vec = _factories[vtype];
                for (const ProtoFactory& fr : func_vec)
                {
                    int size = 1;
                    if ((int)fr.args.size() != size)
                        continue;
                     
                    if (fr.args[0] == std::type_index(typeid(arg)))
                    {
                        fptr = fr.func;
                        cache[vtype] = fptr;
                        break;
                    }
                }
            }
        }

        if (fptr)
            return (*fptr)(arg);

        throw NotFoundException(TRACE_INFO,
              "No factory found for this Value type and arguments.");
    }
};

ValueServer& valueserver();

#define TOKENPASTE(x, y) x ## y
#define TOKENPASTE2(x, y) TOKENPASTE(x, y)
#define DEFINE_VALUE_FACTORY(CTYPE,CREATE,ARG)                       \
                                                                     \
/* This runs when the shared lib is loaded. */                       \
static __attribute__ ((constructor)) void                            \
    TOKENPASTE2(init, __COUNTER__)(void)                             \
{                                                                    \
   valueserver().addFactory(CTYPE, (CreateProto) & (CREATE<ARG>),    \
      std::vector<std::type_index> {std::type_index(typeid(ARG))});  \
}

}
#endif
