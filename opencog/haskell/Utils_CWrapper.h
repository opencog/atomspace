#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atomspace/TruthValue.h>
#include <opencog/atomspace/SimpleTruthValue.h>
#include <opencog/atomspace/CountTruthValue.h>
#include <opencog/atomspace/IndefiniteTruthValue.h>
#include <opencog/atomspace/FuzzyTruthValue.h>
#include <opencog/atomspace/ProbabilisticTruthValue.h>


/**
 * C wrapper of the Pattern Matcher api:
 * It was developed as an interface necessary for haskell bindings.
 * (ghc supports FFI for c libraries)
 * Now, it doesn't have specific code related to Haskell, so this library
 * could be used by another application with same requirements.
 */

extern "C"
{
    using namespace opencog;

    int Utils_toRawType(TruthValuePtr tv
                        , TruthValueType* tv_type
                        , double* parameters);
}

