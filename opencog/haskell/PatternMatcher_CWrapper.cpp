
#include "PatternMatcher_CWrapper.h"
#include "Utils_CWrapper.h"
#include <opencog/query/BindLinkAPI.h>

UUID PatternMatcher_BindLink(AtomSpace* this_ptr, UUID handle)
{
    Handle h(handle);
    return bindlink(this_ptr, h).value();
}

int PatternMatcher_SatisfactionLink(AtomSpace* this_ptr
                            , UUID handle
                            , TruthValueType* tv_type
                            , double* parameters)
{
    Handle h(handle);
    TruthValuePtr tv = satisfaction_link(this_ptr, h);
    return Utils_toRawType(tv,tv_type,parameters);
}

