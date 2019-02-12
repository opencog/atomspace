from libcpp.memory cimport shared_ptr
from opencog.atomspace cimport Type, Value, cValue, cValuePtr

cdef extern from "opencog/atoms/value/PtrValue.h" namespace "opencog":
    cdef cppclass cPtrValue "opencog::PtrValue":
        void* value() const;

    cdef cValuePtr createPtrValue(...)
