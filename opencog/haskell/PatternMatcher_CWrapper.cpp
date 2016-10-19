
#include "PatternMatcher_CWrapper.h"
#include "Utils_CWrapper.h"
#include <opencog/query/BindLinkAPI.h>

Handle* PatternMatcher_BindLink(AtomSpace* this_ptr, Handle* handle)
{
    Handle* res = (Handle*)malloc(sizeof(Handle));
    (*res) = bindlink(this_ptr, *handle);
    return res;
}

int PatternMatcher_SatisfactionLink(AtomSpace* this_ptr
                            , Handle* handle
                            , TruthValueType* tv_type
                            , double* parameters)
{
    TruthValuePtr tv = satisfaction_link(this_ptr, *handle);
    return Utils_toRawType(tv,tv_type,parameters);
}

