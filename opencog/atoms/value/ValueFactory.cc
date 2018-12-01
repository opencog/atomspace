#include "ValueFactory.h"

using namespace opencog;

void ValueServer::addFactory(Type vtype, ValueFactory func,
                             std::vector<std::type_index> args)
{
    ProtoFactory fr = {func, args};

    if (_factories.find(vtype) != _factories.end())
        _factories[vtype].push_back(fr);
    else
        _factories[vtype] = {fr};
}

void ValueServer::addCaster(Type vtype, ValueCaster func)
{
    _vcasters[vtype] = func;
}

ProtoAtomPtr ValueServer::recast(const ProtoAtomPtr& ptr) const
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
