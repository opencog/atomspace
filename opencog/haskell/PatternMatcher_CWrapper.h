
#include <opencog/atomspace/AtomSpace.h>

/**
 * C wrapper of the Pattern Matcher api:
 * An interface necessary for haskell bindings.
 * (ghc supports FFI for c libraries)
 * XXX FIXME: atoms must never be accessed by UUID except by the
 * communication and database layers. The UUID is not meant to be
 * a public interface.
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
    Handle* PatternMatcher_BindLink(AtomSpace* this_ptr, Handle* handle);
    int PatternMatcher_SatisfactionLink(AtomSpace* this_ptr
                                        , Handle* handle
                                        , char** tv_type
                                        , double* parameters);

}

