
#include "PatternMatcher_CWrapper.h"
#include <opencog/query/BindLinkAPI.h>

UUID PatternMatcher_BindLink(AtomSpace* this_ptr, UUID handle)
{
    Handle h(handle);
    return bindlink(this_ptr, h).value();
}

