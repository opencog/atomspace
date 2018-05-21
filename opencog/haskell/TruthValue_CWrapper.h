#include <opencog/atomspace/AtomSpace.h>
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

    /**
     * TruthValue_getFromAtom  Gets the truthvalue of an Atom
     *
     * @param      handle      Handle id of target atom.
     * @param[out] tv_type     TruthValue type.
     * @param[out] parameters  List of parameters of the TV (strenght,confidence,...)
     *
     * @return  0 if success.
     */
    int TruthValue_getFromAtom( Handle* handle
                              , char** tv_type
                              , double* parameters );

    /**
     * TruthValue_setOnAtom    Sets the truthvalue of an Atom
     *
     * @param      handle      Handle id of target atom.
     * @param      type        TruthValue type to be set.
     * @param      parameters  List of parameters of the TV (strenght,confidence,...)
     *
     * @return  0 if success.
     */
    int TruthValue_setOnAtom( Handle* handle
                            , const char* type
                            , double* parameters );

}

