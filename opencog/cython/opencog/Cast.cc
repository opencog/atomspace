

#include <opencog/atoms/base/Atom.h>
#include "Cast.h"

using namespace opencog;

Handle atom_from_the_void(long p) { return *((Handle*) p); }
long void_from_candle(const Handle& hp) { return (long) (&hp); }
long void_from_cptr(Handle* hp) { return (long) (hp); }

void incref(void* ptr) { printf("incref called\n"); Py_INCREF(static_cast<PyObject*>(ptr)); }
void decref(void* ptr) { printf("decref called\n"); Py_DECREF(static_cast<PyObject*>(ptr)); }
