#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/truthvalue/CountTruthValue.h>
#include <opencog/truthvalue/IndefiniteTruthValue.h>
#include <opencog/truthvalue/FuzzyTruthValue.h>
#include <opencog/truthvalue/ProbabilisticTruthValue.h>


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

