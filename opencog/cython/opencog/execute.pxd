from opencog.atomspace cimport cHandle, cValuePtr, cAtomSpace

cdef extern from "opencog/atoms/execution/EvaluationLink.h" namespace "opencog":
    cValuePtr c_evaluate_atom "opencog::EvaluationLink::do_evaluate"(cAtomSpace*, cHandle) except +
