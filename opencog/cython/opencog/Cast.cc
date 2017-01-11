

#include <opencog/atoms/base/Atom.h>
#include "Cast.h"

using namespace opencog;

Handle atom_from_the_void(long p) { return ((Atom*) p)->getHandle(); }
long void_from_candle(const Handle& hp) { return (long) (hp.operator->()); }
long void_from_cptr(Handle* hp) { return (long) (hp->operator->()); }
