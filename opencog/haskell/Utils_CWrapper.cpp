
#include "Utils_CWrapper.h"

int Utils_toRawType(TruthValuePtr tv
                    , TruthValueType* tv_type
                    , double* parameters)
{
switch(tv->getType())
    {
        case SIMPLE_TRUTH_VALUE: {
            parameters[0]=tv->getMean();
            parameters[1]=tv->getConfidence();
            break; }
        case COUNT_TRUTH_VALUE: {
            parameters[0]=tv->getMean();
            parameters[1]=tv->getCount();
            parameters[2]=tv->getConfidence();
            break; }
        case INDEFINITE_TRUTH_VALUE: {
            IndefiniteTruthValuePtr itv = IndefiniteTVCast(tv);
            parameters[0]=itv->getMean();
            parameters[1]=itv->getL();
            parameters[2]=itv->getU();
            parameters[3]=itv->getConfidenceLevel();
            parameters[4]=itv->getDiff();
            break; }
        case FUZZY_TRUTH_VALUE: {
            parameters[0]=tv->getMean();
            parameters[1]=tv->getConfidence();
            break; }
        case PROBABILISTIC_TRUTH_VALUE: {
            parameters[0]=tv->getMean();
            parameters[1]=tv->getCount();
            parameters[2]=tv->getConfidence();
            break; }
        default:
            throw RuntimeException(TRACE_INFO,
                "Invalid TruthValue Type.");
            break;
    }
    *tv_type = tv->getType();
    return 0;
}
