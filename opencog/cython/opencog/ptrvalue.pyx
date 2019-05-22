from cpython.ref cimport Py_INCREF, Py_DECREF

cdef class PtrValue(Value):

    def __init__(self, obj = None, ptr_holder = None):
        if obj is not None:
            cvalue = create_ptr_value_from_python_object(obj)
            super(PtrValue, self).__init__(PtrHolder.create(<shared_ptr[void]&>cvalue))
        else:
            super(PtrValue, self).__init__(ptr_holder)

    def value(self):
        return <object>((<cPtrValue*>self.get_c_value_ptr().get()).value())

cdef void decref(void* obj):
    Py_DECREF(<object>obj)

cdef cValuePtr create_ptr_value_from_python_object(object obj):
    Py_INCREF(obj)
    return createPtrValue(<void*>obj, decref)
