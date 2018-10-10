#include "ValueFactory.h"

using namespace opencog;

void ValueFactory::register_factory(Type vtype, CreateProto func, std::vector<std::type_index> args)
{
    FuncRegister fr = {func, args};

    if(func_register.find(vtype) != func_register.end()) {
        func_register[vtype].push_back(fr);
    } else {
        func_register[vtype] = {fr};
    }
}

ValueFactory& opencog::valuefactory()
{
    static std::unique_ptr<ValueFactory> instance(new ValueFactory());
    return *instance;
}
