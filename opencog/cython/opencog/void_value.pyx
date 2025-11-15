
def createVoidValue():
    """Create and return the VoidValue singleton instance."""
    cdef cValuePtr c_ptr = c_createVoidValue[int]()
    return VoidValue(PtrHolder.create(c_ptr))

cdef class VoidValue(Value):

    def to_list(self):
        # VoidValue has no data, return empty list
        return []
