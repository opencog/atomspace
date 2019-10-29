#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/truthvalue/CountTruthValue.h>
#include <opencog/atoms/truthvalue/IndefiniteTruthValue.h>
#include <opencog/atoms/truthvalue/FuzzyTruthValue.h>
#include <opencog/atoms/truthvalue/ProbabilisticTruthValue.h>


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

    /**
     * TruthValuePtr_fromRaw    Helper function that creates TruthValuePtr from RawTV
     *
     * @param      type        TruthValue type
     * @param      parameters  List of parameters of the TV (strenght,confidence,...)
     *
     * @return  TruthValuePtr
     */
    TruthValuePtr TruthValuePtr_fromRaw(const char* type, double* parameters);

    /**
     * PTruthValuePtr_fromRaw    Function needed for GroundedPredicate Haskell binding that converts RawTV to TruthValuePtr*
     *
     * @param      type        TruthValue type
     * @param      parameters  List of parameters of the TV (strenght,confidence,...)
     *
     * @return  TruthValuePtr*
     */
    TruthValuePtr* PTruthValuePtr_fromRaw(const char* type, double* parameters);

}

