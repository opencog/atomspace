from libcpp.set cimport set
from libcpp.vector cimport vector
from opencog.atomspace cimport cHandle, cAtomSpace


cdef extern from "opencog/rule-engine/forwardchainer/ForwardChainer.h" namespace "opencog":
    cdef cppclass cForwardChainer "opencog::ForwardChainer":
        cForwardChainer(cAtomSpace& kb_as,
                        cAtomSpace& rb_as,
                        const cHandle& rbs,
                        const cHandle& source,
                        const cHandle& vardecl,
                        const vector[cHandle]& focus_set) except +

        void do_chain()
        set[cHandle] get_chaining_result() const

