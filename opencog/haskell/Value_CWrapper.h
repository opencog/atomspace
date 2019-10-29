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
     * FloatValue_toRawType Turns a FloatValuePtr ito
     *                      something that haskell interface
     *                      can work with
     *
     * @param      ptr        FloatValuePtr to convert
     * @param[out] valuetype  The Type of the FloatValue as a String
     * @param[out] parameters List of Doubles, the value of the FloatValue
     *
     * @return 0 if success
     */
    int FloatValue_toRaw(FloatValuePtr ptr
                        , char** valuetype
                        , double* parameters);

    /**
     * FloatValue_getFromAtom  Gets the truthvalue of an Atom
     *
     * @param      handle      Handle id of target atom.
     * @param[out] valuetype   FloatValue type.
     * @param[out] parameters  List of Doubles, the value of the FloatValue
     *
     * @return  0 if success.
     */
    int FloatValue_getFromAtom( Handle* atom
                              , Handle* key
                              , char** valuetype
                              , double* parameters );
    /**
     * FloatValue_setOnAtom    Sets the truthvalue of an Atom
     *
     * @param      handle      Handle id of target atom.
     * @param      type        FloatValue type to be set.
     * @param      parameters  List of Doubles, the value of the FloatValue
     *
     * @return  0 if success.
     */
    int FloatValue_setOnAtom( Handle* atom
                            , Handle* key
                            , const char* valuetype
                            , double* parameters
                            , int length);
}

