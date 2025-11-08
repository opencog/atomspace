
cdef class VoidValue(Value):

    def to_list(self):
        # VoidValue has no data, return empty list
        return []
