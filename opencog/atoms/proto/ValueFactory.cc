#include "ValueFactory.h"

using namespace opencog;

void ValueFactory::addFactory(Type vtype, CreateProto func, std::vector<std::type_index> args)
{
    FuncRegister fr = {func, args};

    if (func_register.find(vtype) != func_register.end()) {
        func_register[vtype].push_back(fr);
    } else {
        func_register[vtype] = {fr};
    }
}

void ValueFactory::addCreator(Type vtype, CreateProto func)
{
    cast_register[vtype] = func;
}

ProtoAtomPtr ValueFactory::recreate(ProtoAtomPtr ptr)
{
    Type vtype = ptr->get_type();
    if (cast_register.find(vtype) != cast_register.end())
        return (*cast_register[vtype])(ptr);
    else
        THROW_ERROR;
}

ValueFactory& opencog::valuefactory()
{
    static std::unique_ptr<ValueFactory> instance(new ValueFactory());
    return *instance;
}
