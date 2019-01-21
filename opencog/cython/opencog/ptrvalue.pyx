from opencog.atomspace cimport get_value_ptr
from cpython.ref cimport Py_INCREF, Py_DECREF

cdef Value wrapPtrValue(cValuePtr shared_ptr):
    """Factory method to construct PtrValue from C++ ValuePtr (see
    http://docs.cython.org/en/latest/src/userguide/extension_types.html#instantiation-from-existing-c-c-pointers
    for example)"""
    cdef PtrValueClass value = PtrValueClass.__new__(PtrValueClass)
    value.shared_ptr = shared_ptr
    return value

cdef class PtrValueClass(Value):
    def value(self):
        return <object>(<cPtrValue*>get_value_ptr(self)).value()

cdef void decref(void* obj):
    Py_DECREF(<object>obj)

def PtrValue(obj):
    Py_INCREF(obj)
    return wrapPtrValue(createPtrValue(<void*>obj, decref))
