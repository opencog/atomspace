from opencog.atomspace cimport cHandle, tv_ptr, cAtomSpace

ctypedef size_t cSize

cdef extern from "opencog/cython/opencog/BindlinkStub.h" namespace "opencog":
    # C++: 
    #   Handle stub_bindlink(AtomSpace*, Handle);
    #
    cdef cHandle c_stub_bindlink "stub_bindlink" (cAtomSpace*, cHandle)
    cdef cHandle c_execute_atom "do_execute"(cAtomSpace*, cHandle)


cdef extern from "opencog/query/BindLinkAPI.h" namespace "opencog":
    # C++: 
    #   Handle bindlink(AtomSpace*, Handle, size_t);
    #   Handle af_bindlink(AtomSpace*, Handle);
    #   TruthValuePtr satisfaction_link(AtomSpace*, Handle);
    #   Handle satisfying_set(AtomSpace*, Handle, size_t);
    #
    cdef cHandle c_bindlink "bindlink" (cAtomSpace*, cHandle, cSize)
    cdef cHandle c_af_bindlink "af_bindlink" (cAtomSpace*, cHandle)
    cdef tv_ptr c_satisfaction_link "satisfaction_link" (cAtomSpace*, cHandle)
    cdef cHandle c_satisfying_set "satisfying_set" (cAtomSpace*, cHandle, cSize)

cdef extern from "opencog/atoms/execution/EvaluationLink.h" namespace "opencog":
    tv_ptr c_evaluate_atom "opencog::EvaluationLink::do_evaluate"(cAtomSpace*, cHandle)
