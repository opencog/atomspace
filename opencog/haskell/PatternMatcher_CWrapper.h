
#include <opencog/atomspace/AtomSpace.h>

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

    /**
     * PatternMatcher_bindlink Redirect the call to bindlink.
     *
     * @param  this_ptr  Pointer to AtomSpace instance.
     * @param  handle    Handle id of the bind link pattern.
     *
     * @return  Handle id of the atom.
     */
    UUID PatternMatcher_BindLink(AtomSpace* this_ptr, UUID handle);
    int PatternMatcher_SatisfactionLink(AtomSpace* this_ptr
                                        , UUID handle
                                        , TruthValueType* tv_type
                                        , double* parameters);

}

