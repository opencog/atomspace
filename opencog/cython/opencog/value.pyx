from cpython.object cimport Py_EQ, Py_NE

cdef Value createProtoAtom(cValuePtr shared_ptr):
    """Factory method to construct Value from C++ ValuePtr (see
    http://docs.cython.org/en/latest/src/userguide/extension_types.html#instantiation-from-existing-c-c-pointers
    for example)"""
    cdef Value proto_atom = Value.__new__(Value)
    proto_atom.shared_ptr = shared_ptr
    return proto_atom

cdef list vector_of_doubles_to_list(const vector[double]* cpp_vector):
    list = []
    it = cpp_vector.const_begin()
    while it != cpp_vector.const_end():
        list.append(deref(it))
        inc(it)
    return list

cdef list vector_of_strings_to_list(const vector[string]* cpp_vector):
    list = []
    it = cpp_vector.const_begin()
    while it != cpp_vector.const_end():
        list.append((<bytes>deref(it).c_str()).decode('UTF-8'))
        inc(it)
    return list

cdef list vector_of_values_to_list(const vector[cValuePtr]* cpp_vector):
    list = []
    it = cpp_vector.const_begin()
    cdef cValuePtr value
    while it != cpp_vector.const_end():
        value = deref(it)
        if is_a(deref(value).get_type(), types.Value):
            list.append(createProtoAtom(value))
        else:
            # TODO: Support Atoms as members of LinkValue requires inheriting
            # Atom from Value and constructor to create Atom from cHandle.
            raise TypeError('Only Values are supported '
                            'as members of LinkValue')
        inc(it)
    return list

cdef cValue* get_value_ptr(Value protoAtom):
    """Return plain C++ Value pointer, raise AttributeError if
    pointer is nullptr"""
    cdef cValue* ptr = protoAtom.shared_ptr.get()
    if ptr == NULL:
        raise AttributeError('Value contains NULL reference')
    else:
        return ptr

cdef class Value:
    """C++ Value object wrapper for Python clients"""

    property type:
         def __get__(self):
             return get_value_ptr(self).get_type()

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
        if self.is_a(types.FloatSeqValue):
            return vector_of_doubles_to_list(
                &((<cFloatSeqValue*>get_value_ptr(self)).value()))
        elif self.is_a(types.StringValue):
            return vector_of_strings_to_list(
                &((<cStringValue*>get_value_ptr(self)).value()))
        elif self.is_a(types.LinkValue):
            return vector_of_values_to_list(
                &((<cLinkValue*>get_value_ptr(self)).value()))
        else:
            raise TypeError('Type {} is not supported'.format(self.type()))

    def long_string(self):
        return get_value_ptr(self).to_string().decode('UTF-8')

    def short_string(self):
        return get_value_ptr(self).to_short_string().decode('UTF-8')

    def __str__(self):
        return self.short_string()

    def __repr__(self):
        return self.long_string()

    def __richcmp__(self, other, op):
        if not isinstance(other, Value):
            raise TypeError('Value cannot be compared with {}'
                            .format(type(other)))
        cdef cValue* self_ptr = get_value_ptr(<Value>self)
        cdef cValue* other_ptr = get_value_ptr(<Value>other)
        if op == Py_EQ:
            return deref(self_ptr) == deref(other_ptr)
        elif op == Py_NE:
            return deref(self_ptr) != deref(other_ptr)
        else:
            raise TypeError('Value can be compared using '
                            + 'Py_EQ and Py_NE only')
