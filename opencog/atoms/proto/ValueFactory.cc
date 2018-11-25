#include "ValueFactory.h"

using namespace opencog;

void ValueFactory::addFactory(Type vtype, CreateProto func, std::vector<std::type_index> args)
{
    ProtoFactory fr = {func, args};

    if (_factories.find(vtype) != _factories.end()) {
        _factories[vtype].push_back(fr);
    } else {
        _factories[vtype] = {fr};
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
