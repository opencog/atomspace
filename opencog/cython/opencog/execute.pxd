from opencog.atomspace cimport cHandle, tv_ptr, cAtomSpace

cdef extern from "opencog/atoms/execution/EvaluationLink.h" namespace "opencog":
    tv_ptr c_evaluate_atom "opencog::EvaluationLink::do_evaluate"(cAtomSpace*, cHandle) except +
