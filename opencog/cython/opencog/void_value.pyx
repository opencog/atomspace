
def createVoidValue():
    """Create and return the VoidValue singleton instance."""
    cdef cValuePtr c_ptr = c_createVoidValue[int]()
    cdef VoidValue instance = VoidValue.__new__(VoidValue)
    instance.shared_ptr = c_ptr
    return instance

cdef class VoidValue(Value):

    def to_list(self):
        # VoidValue has no data, return empty list
        return []
