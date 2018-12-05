#ifndef _VALUE_FACTORY_H_
#define _VALUE_FACTORY_H_

#include "Value.h"
#include <opencog/atoms/base/Handle.h>
#include <opencog/util/exceptions.h>

#include <map>
#include <typeinfo>
#include <typeindex>
#include <vector>

namespace opencog
{
using ValueFactory = ValuePtr (*) (...);
using ValueCaster = ValuePtr (*) (const ValuePtr&);


class ValueServer
{
    friend ValueServer& valueserver();
private:
    ValueServer() {}
    
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
    template <typename TYP, typename ARG>
    ValuePtr create(TYP vtype, ARG arg) const
    {
        // Look up the factory only once; cache the result.
        static ValueFactory fptr = nullptr;

        if (nullptr == fptr)
        {
            try
            {
                // First, find the list of factories for this type.
                std::vector<ProtoFactory> func_vec = _factories.at(vtype);

                // Second, find the matching arglist.
                for (const ProtoFactory& fr : func_vec)
                {
                    // At this time, only one arg is supported. FIXME
                    int size = 1;
                    if ((int) fr.args.size() != size)
                        continue;
                     
                    if (fr.args[0] == std::type_index(typeid(arg)))
                    {
                        fptr = fr.func;
                        break;
                    }
                }
            }
            catch(...) {}
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
   valueserver().addFactory(CTYPE, (ValueFactory) & (CREATE<ARG>),   \
      std::vector<std::type_index> {std::type_index(typeid(ARG))});  \
}

}
#endif
