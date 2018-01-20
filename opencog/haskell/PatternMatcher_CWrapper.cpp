
#include "PatternMatcher_CWrapper.h"
#include "Value_CWrapper.h"
#include <opencog/query/BindLinkAPI.h>

Handle* PatternMatcher_BindLink(AtomSpace* this_ptr, Handle* handle)
{
    Handle* res = (Handle*)malloc(sizeof(Handle));
    (*res) = bindlink(this_ptr, *handle);
    return res;
}

int PatternMatcher_SatisfactionLink(AtomSpace* this_ptr
                            , Handle* handle
                            , char** tv_type
                            , double* parameters)
{
    TruthValuePtr tv = satisfaction_link(this_ptr, *handle);
    return FloatValue_toRaw(tv,tv_type,parameters);
}

