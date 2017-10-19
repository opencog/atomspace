#include "TruthValue_CWrapper.h"
#include "Value_CWrapper.h"

int TruthValue_getFromAtom( Handle* atom
                          , char** tv_type
                          , double* parameters )
{
    Handle h = *atom;
    if(h == Handle::UNDEFINED)
        return -1;
    TruthValuePtr tv = h->getTruthValue();

    return FloatValue_toRaw(tv,tv_type,parameters);
}

int TruthValue_setOnAtom( Handle* atom
                        , const char* type
                        , double* parameters )
{
    Handle h = *atom;
    if (!h) // Invalid UUID parameter.
        return -1;

    if (strcmp(type,"SimpleTruthValue") == 0) {
        h->setTruthValue(SimpleTruthValue::createTV(parameters[0],parameters[1]));
    }
    else
    if (strcmp(type,"CountTruthValue") == 0) {
        h->setTruthValue(CountTruthValue::createTV(parameters[0]
                                                  ,parameters[1]
                                                  ,parameters[2]));
    }
    else
    if (strcmp(type,"IndefiniteTruthValue") == 0) {
        IndefiniteTruthValuePtr iptr =
            IndefiniteTruthValue::createITV(parameters[1]
                                           ,parameters[2]
                                           ,parameters[3]);
        // iptr->setMean(parameters[0]);
        // iptr->setDiff(parameters[4]);
        h->setTruthValue(std::static_pointer_cast<const TruthValue>(iptr));
    }
    else
    if (strcmp(type,"FuzzyTruthValue") == 0) {
        h->setTruthValue(FuzzyTruthValue::createTV(parameters[0],parameters[1]));
    }
    else
    if (strcmp(type,"ProbabilisticTruthValue") == 0) {
        h->setTruthValue(ProbabilisticTruthValue::createTV(parameters[0]
                                                          ,parameters[1]
                                                          ,parameters[2]));
    }
    else
        throw InvalidParamException(TRACE_INFO,
                ("Invalid TruthValue Type: " + std::string(type)).c_str());
    return 0;
}
