#include <opencog/atomspace/AtomSpace.h>

/**
 * C wrapper fro truth values.
 * An interface necessary for haskell bindings.
 * (ghc supports FFI for c libraries)
 */

extern "C"
{
    using namespace opencog;

    /**
     * FloatSeqValue_toRawType Turns a FloatSeqValuePtr ito
     *                      something that haskell interface
     *                      can work with
     *
     * @param      ptr        FloatSeqValuePtr to convert
     * @param[out] valuetype  The Type of the FloatSeqValue as a String
     * @param[out] parameters List of Doubles, the value of the FloatSeqValue
     *
     * @return 0 if success
     */
    int FloatSeqValue_toRaw(FloatSeqValuePtr ptr
                           , char** valuetype
                           , double* parameters);

    /**
     * FloatSeqValue_getFromAtom  Gets the truthvalue of an Atom
     *
     * @param      handle      Handle id of target atom.
     * @param[out] valuetype   FloatSeqValue type.
     * @param[out] parameters  List of Doubles, the value of the FloatSeqValue
     *
     * @return  0 if success.
     */
    int FloatSeqValue_getFromAtom( Handle* atom
                                 , Handle* key
                                 , char** valuetype
                                 , double* parameters );
    /**
     * FloatSeqValue_setOnAtom    Sets the truthvalue of an Atom
     *
     * @param      handle      Handle id of target atom.
     * @param      type        FloatSeqValue type to be set.
     * @param      parameters  List of Doubles, the value of the FloatSeqValue
     *
     * @return  0 if success.
     */
    int FloatSeqValue_setOnAtom( Handle* atom
                               , Handle* key
                               , const char* valuetype
                               , double* parameters
                               , int length);
}

