#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/truthvalue/CountTruthValue.h>
#include <opencog/truthvalue/IndefiniteTruthValue.h>
#include <opencog/truthvalue/FuzzyTruthValue.h>
#include <opencog/truthvalue/ProbabilisticTruthValue.h>


/**
 * C wrapper fro truth values.
 * An interface necessary for haskell bindings.
 * (ghc supports FFI for c libraries)
 */

extern "C"
{
    using namespace opencog;

    int Utils_toRawType(TruthValuePtr tv
                        , TruthValueType* tv_type
                        , double* parameters);
}

