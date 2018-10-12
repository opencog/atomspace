#ifndef _VALUE_FACTORY_H_
#define _VALUE_FACTORY_H_

#include "ProtoAtom.h"
#include <opencog/atoms/base/Handle.h>

#include <exception>
#include <map>
#include <typeinfo>
#include <typeindex>
#include <vector>

namespace opencog
{
using CreateProto = ProtoAtomPtr (*) (...);

struct FuncRegister
{
    CreateProto func;
    std::vector<std::type_index> args;
};

class ValueFactory
{
private:

#define THROW_ERROR \
    (throw std::invalid_argument("There was no function found for the given type."))

    ValueFactory() {}

    std::map<Type, std::vector<FuncRegister>> func_register;

public:

    friend ValueFactory& valuefactory();

    /** Registers the creator functions.
     *  @param vtype The value type.
     *  @param func a pointer to the creator function of the value.
     *  @param args  an ordered vector the types of arguments the creator takes.
     */
     void register_factory(Type vtype, CreateProto func, std::vector<std::type_index> args);


     /** Does dynamic dispatching of the appropriate create functions which matches argument provided.
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

        if(cache.find(vtype) != cache.end()) {
            fptr = cache[vtype];

        } else {
            if(func_register.find(vtype) != func_register.end()) {
                std::vector<FuncRegister> func_vec = func_register[vtype];
                for(FuncRegister fr : func_vec) {
                    int size = 1;
                    if((int)fr.args.size() != size)
                        continue;

                    bool match = false;
                    if(fr.args[0] == std::type_index(typeid(arg)))
                        match = true;

                    if(match) {
                        fptr = fr.func;
                        cache[vtype] = fptr;
                        break;
                    }
                }
            }
        }

        if(fptr)
            return (*fptr)(arg);

        THROW_ERROR;
    }

    // Multitype arguments or Mulit arguments are explicitly hanled for now.
    // add a creator function here if your Value constructor takes multiple arguments.
};

ValueFactory& valuefactory();
}
#endif
