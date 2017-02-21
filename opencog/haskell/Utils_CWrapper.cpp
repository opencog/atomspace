
#include "Utils_CWrapper.h"

int Utils_toRawType(TruthValuePtr tv
                    , Type* tv_type
                    , double* parameters)
{
    Type tvt = tv->getType();
    if (tvt == SIMPLE_TRUTH_VALUE){
        parameters[0]=tv->getMean();
        parameters[1]=tv->getConfidence();
    }
    else
    if (tvt == COUNT_TRUTH_VALUE){
        parameters[0]=tv->getMean();
        parameters[1]=tv->getCount();
        parameters[2]=tv->getConfidence();
    }
    else
    if (tvt == INDEFINITE_TRUTH_VALUE){
        IndefiniteTruthValuePtr itv = IndefiniteTVCast(tv);
        parameters[0]=itv->getMean();
        parameters[1]=itv->getL();
        parameters[2]=itv->getU();
        parameters[3]=itv->getConfidenceLevel();
        parameters[4]=itv->getDiff();
    }
    else
    if (tvt == FUZZY_TRUTH_VALUE){
        parameters[0]=tv->getMean();
        parameters[1]=tv->getConfidence();
    }
    else
    if (tvt == PROBABILISTIC_TRUTH_VALUE){
        parameters[0]=tv->getMean();
        parameters[1]=tv->getCount();
        parameters[2]=tv->getConfidence();
    }
    else {
        throw RuntimeException(TRACE_INFO,
            "Invalid TruthValue Type.");
    }
    *tv_type = tvt;
    return 0;
}
