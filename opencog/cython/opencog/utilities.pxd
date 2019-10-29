from opencog.atomspace cimport cAtomSpace

cdef extern from "opencog/cython/opencog/Utilities.h" namespace "opencog":
    # C++:
    #
    #   initialize_python(AtomSpace*);
    #   void finalize_python();
    #
    cdef void c_initialize_python "opencog::initialize_python" (cAtomSpace*)
    cdef void c_finalize_python "opencog::finalize_python" ()
