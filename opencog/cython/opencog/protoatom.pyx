from cpython.object cimport Py_EQ, Py_NE

cdef ProtoAtom createProtoAtom(cProtoAtomPtr shared_ptr):
    """Factory method to construct ProtoAtom from C++ ProtoAtomPtr (see
    http://docs.cython.org/en/latest/src/userguide/extension_types.html#instantiation-from-existing-c-c-pointers
    for example)"""
    cdef ProtoAtom proto_atom = ProtoAtom.__new__(ProtoAtom)
    proto_atom.shared_ptr = shared_ptr
    return proto_atom

cdef class ProtoAtom:
    """C++ ProtoAtom object wrapper for Python clients"""

    cdef cProtoAtom* get_ptr(self):
        """Return plain C++ ProtoAtom pointer, raise AttributeError if
        pointer is nullptr"""
        cdef cProtoAtom* ptr = self.shared_ptr.get()
        if ptr == NULL:
            raise AttributeError('ProtoAtom contains NULL reference')
        else:
            return ptr

    property type:
         def __get__(self):
             return self.get_ptr().get_type()

    property type_name:
        def __get__(self):
            return get_type_name(self.type)

    def is_atom(self):
        return is_a(self.type, types.Node)

    def is_node(self):
        return is_a(self.type, types.Node)

    def is_link(self):
        return is_a(self.type, types.Link)

    def is_a(self, type):
        return is_a(self.type, type)

    def to_list(self):
        cdef const vector[double]* doubleValues;
        if (self.is_a(types.FloatValue)):
            list = []
            doubleValues = &((<cFloatValue*>self.get_ptr()).value())
            it = doubleValues.const_begin()
            while it != doubleValues.const_end():
                list.append(deref(it))
                inc(it)
            return list
        else:
            raise TypeError('Type {} is not supported'.format(self.type()))

    def long_string(self):
        return self.get_ptr().to_string().decode('UTF-8')

    def short_string(self):
        return self.get_ptr().to_short_string().decode('UTF-8')

    def __str__(self):
        return self.short_string()

    def __repr__(self):
        return self.long_string()

    def __richcmp__(self, other, op):
        if not isinstance(other, ProtoAtom):
            raise TypeError('ProtoAtom cannot be compared with {}'
                            .format(type(other)))
        cdef cProtoAtom* self_ptr = (<ProtoAtom>self).get_ptr()
        cdef cProtoAtom* other_ptr = (<ProtoAtom>other).get_ptr()
        if op == Py_EQ:
            return deref(self_ptr) == deref(other_ptr)
        elif op == Py_NE:
            return deref(self_ptr) != deref(other_ptr)
        else:
            raise TypeError('ProtoAtom can be compared using '
                            + 'Py_EQ and Py_NE only')
