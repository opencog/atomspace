from opencog.atomspace cimport cHandle, tv_ptr, cAtomSpace

ctypedef size_t cSize

cdef extern from "opencog/atoms/execution/EvaluationLink.h" namespace "opencog":
    tv_ptr c_evaluate_atom "opencog::EvaluationLink::do_evaluate"(cAtomSpace*, cHandle)

cdef extern from "opencog/cython/opencog/BindlinkStub.h" namespace "opencog":
    cdef cHandle c_execute_atom "do_execute"(cAtomSpace*, cHandle) except +
