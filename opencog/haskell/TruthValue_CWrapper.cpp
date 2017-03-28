#include "TruthValue_CWrapper.h"


int TruthValue_toRawType(TruthValuePtr tv
                        , char** tv_type
                        , double* parameters)
{
    const std::string & tvt = classserver().getTypeName(tv->getType());

    *tv_type = (char*) malloc(sizeof(char) * (tvt.length()+1));
    if(! *tv_type)
        throw RuntimeException(TRACE_INFO,"Failed malloc.");
    std::strcpy(*tv_type, tvt.c_str());

    if (strcmp(*tv_type,"SimpleTruthValue") == 0){
        parameters[0]=tv->getMean();
        parameters[1]=tv->getConfidence();
    }
    else
    if (strcmp(*tv_type,"CountTruthValue") == 0){
        parameters[0]=tv->getMean();
        parameters[1]=tv->getCount();
        parameters[2]=tv->getConfidence();
    }
    else
    if (strcmp(*tv_type,"IndefiniteTruthValue") == 0){
        IndefiniteTruthValuePtr itv = IndefiniteTVCast(tv);
        parameters[0]=itv->getMean();
        parameters[1]=itv->getL();
        parameters[2]=itv->getU();
        parameters[3]=itv->getConfidenceLevel();
        parameters[4]=itv->getDiff();
    }
    else
    if (strcmp(*tv_type,"FuzzyTruthValue") == 0){
        parameters[0]=tv->getMean();
        parameters[1]=tv->getConfidence();
    }
    else
    if (strcmp(*tv_type,"ProbabilisticTruthValue") == 0){
        parameters[0]=tv->getMean();
        parameters[1]=tv->getCount();
        parameters[2]=tv->getConfidence();
    }
    else {
        throw RuntimeException(TRACE_INFO,
                ("Invalid TruthValue Type: " + tvt).c_str());
    }

    return 0;
}

int TruthValue_getFromAtom( Handle* atom
                          , char** tv_type
                          , double* parameters )
{
    Handle h = *atom;
    if(h == Handle::UNDEFINED)
        return -1;
    TruthValuePtr tv = h->getTruthValue();

    return TruthValue_toRawType(tv,tv_type,parameters);
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
                                                  ,parameters[2]
                                                  ,parameters[1]));
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
        double count = FuzzyTruthValue::confidenceToCount(parameters[1]);
        h->setTruthValue(FuzzyTruthValue::createTV(parameters[0],count));
    }
    else
    if (strcmp(type,"ProbabilisticTruthValue") == 0) {
        h->setTruthValue(ProbabilisticTruthValue::createTV(parameters[0]
                                                          ,parameters[2]
                                                          ,parameters[1]));
    }
    else
        throw InvalidParamException(TRACE_INFO,
                ("Invalid TruthValue Type: " + std::string(type)).c_str());
    return 0;
}
