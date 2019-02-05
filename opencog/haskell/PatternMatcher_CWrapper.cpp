
#include "PatternMatcher_CWrapper.h"
#include "Value_CWrapper.h"
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>

Handle* PatternMatcher_BindLink(AtomSpace* this_ptr, Handle* handle)
{
    Handle* res = (Handle*)malloc(sizeof(Handle));
    (*res) = HandleCast((*handle)->execute(this_ptr));
    return res;
}

int PatternMatcher_SatisfactionLink(AtomSpace* this_ptr
                            , Handle* handle
                            , char** tv_type
                            , double* parameters)
{
    TruthValuePtr tv = (*handle)->evaluate(this_ptr);
    return FloatValue_toRaw(tv,tv_type,parameters);
}
