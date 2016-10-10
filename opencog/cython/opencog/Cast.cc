

#include <opencog/atoms/base/Atom.h>
#include "Cast.h"

using namespace opencog;

Handle atom_from_the_void(void* p) { return Handle(AtomPtr((Atom*) p)); }
