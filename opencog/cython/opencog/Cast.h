

#include <opencog/atoms/base/Handle.h>
#include "../PyIncludeWrapper.h"

using namespace opencog;
Handle atom_from_the_void(long p);
long   void_from_candle(const Handle& hp);
long   void_from_cptr(Handle* hp);
void incref(void* ptr);
void decref(void* ptr);
