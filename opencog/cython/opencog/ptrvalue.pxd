from libcpp.memory cimport shared_ptr
from opencog.atomspace cimport Type, Value, cValue, cValuePtr

cdef Value wrapPtrValue(cValuePtr shared_ptr)

cdef extern from "opencog/atoms/value/PtrValue.h" namespace "opencog":
    cdef cppclass cPtrValue "opencog::PtrValue":
        void* value() const;

    cdef cValuePtr createPtrValue(...)
