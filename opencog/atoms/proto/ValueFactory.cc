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

void ValueFactory::addCaster(Type vtype, ValueCaster func)
{
    _vcasters[vtype] = func;
}

ProtoAtomPtr ValueFactory::recast(ProtoAtomPtr ptr)
{
    Type vtype = ptr->get_type();
    if (_vcasters.find(vtype) != _vcasters.end())
        return (*_vcasters[vtype])(ptr);
    else
        return ptr;
}

ValueFactory& opencog::valuefactory()
{
    static std::unique_ptr<ValueFactory> instance(new ValueFactory());
    return *instance;
}
