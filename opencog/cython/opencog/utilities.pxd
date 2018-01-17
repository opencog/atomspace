from opencog.atomspace cimport cAtomSpace

cdef extern from "opencog/cython/opencog/Utilities.h" namespace "opencog":
    # C++:
    #
    #   initialize_opencog(AtomSpace*);
    #   void finalize_opencog();
    #
    cdef void c_initialize_opencog "opencog::initialize_opencog" (cAtomSpace*)
    cdef void c_finalize_opencog "opencog::finalize_opencog" ()
