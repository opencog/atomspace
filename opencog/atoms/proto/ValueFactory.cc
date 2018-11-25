#include "ValueFactory.h"

using namespace opencog;

void ValueServer::addFactory(Type vtype, ValueFactory func, std::vector<std::type_index> args)
{
    ProtoFactory fr = {func, args};

    if (_factories.find(vtype) != _factories.end()) {
        _factories[vtype].push_back(fr);
    } else {
        _factories[vtype] = {fr};
    }
}

void ValueServer::addCaster(Type vtype, ValueCaster func)
{
    _vcasters[vtype] = func;
}

ProtoAtomPtr ValueServer::recast(ProtoAtomPtr ptr)
{
    Type vtype = ptr->get_type();
    if (_vcasters.find(vtype) != _vcasters.end())
        return (*_vcasters[vtype])(ptr);
    else
        return ptr;
}

ValueServer& opencog::valueserver()
{
    static std::unique_ptr<ValueServer> instance(new ValueServer());
    return *instance;
}
