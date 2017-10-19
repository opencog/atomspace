#include "Value_CWrapper.h"

int FloatValue_getFromAtom( Handle* atom
                          , Handle* key
                          , char** valuetype
                          , double* parameters )
{
    Handle a = *atom;
    Handle k = *key;
    if(a == Handle::UNDEFINED || k == Handle::UNDEFINED)
        return -1;
    FloatValuePtr ptr = FloatValueCast(a->getValue(k));

    if (ptr == NULL)
        return -1;

    return FloatValue_toRaw(ptr,valuetype,parameters);
}

int FloatValue_toRaw(FloatValuePtr ptr
                    , char** valuetype
                    , double* parameters)
{
    const std::string & type = classserver().getTypeName(ptr->getType());

    *valuetype = (char*) malloc(sizeof(char) * (type.length()+1));
    if(! *valuetype)
        throw RuntimeException(TRACE_INFO,"Failed malloc.");
    std::strcpy(*valuetype, type.c_str());

    const std::vector<double> val = ptr->value();
    std::copy(val.begin(),val.end(),parameters);

    return 0;
}


int FloatValue_setOnAtom( Handle* atom
                        , Handle* key
                        , const char* valuetype
                        , double* parameters
                        , int length)
{
    Handle a = *atom;
    Handle k = *key;
    if(a == Handle::UNDEFINED || k == Handle::UNDEFINED)
        return -1;

    Type type = classserver().getType(std::string(valuetype));

    std::vector<double> vec(parameters,parameters+length);

    ProtoAtomPtr ptr = createFloatValue(type,vec);

    if (ptr == NULL)
        return -1;

    a->setValue(k,ptr);

    return 0;
}
