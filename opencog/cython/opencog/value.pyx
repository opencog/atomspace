from cpython.object cimport Py_EQ, Py_NE

cdef class ValuePtr:
    """C++ ValuePtr object wrapper for Python clients. Cython cannot create
    Python object constructor which gets C++ pointer. This class is used to
    wrap pointer and make it possible to initialize Value in usual
    constructor (see
    http://docs.cython.org/en/latest/src/userguide/extension_types.html#instantiation-from-existing-c-c-pointers)."""

    @staticmethod
    cdef ValuePtr create(cValuePtr shared_ptr):
        """Factory method to construct ValuePtr from C++ cValuePtr"""
        cdef ValuePtr value_ptr = ValuePtr.__new__(ValuePtr)
        value_ptr.shared_ptr = shared_ptr
        return value_ptr

cdef class Value:
    """C++ Value object wrapper for Python clients"""

    @staticmethod
    cdef Value create(cValuePtr shared_ptr):
        """Factory method to construct Value from C++ cValuePtr using ValuePtr
        instance."""
        return Value(ValuePtr.create(shared_ptr))

    def __init__(self, value_ptr):
        if (<ValuePtr>value_ptr).shared_ptr.get() == NULL:
            raise AttributeError('ValuePtr contains NULL reference')
        self.value_ptr = value_ptr

    cdef cValuePtr get_c_value_ptr(self):
        """Return C++ ValuePtr instance"""
        return self.value_ptr.shared_ptr

    property type:
         def __get__(self):
             return self.get_c_value_ptr().get().get_type()

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
        raise TypeError('Type {} is not supported'.format(self.type()))

    def long_string(self):
        return self.get_c_value_ptr().get().to_string().decode('UTF-8')

    def short_string(self):
        return self.get_c_value_ptr().get().to_short_string().decode('UTF-8')

    def __str__(self):
        return self.short_string()

    def __repr__(self):
        return self.long_string()
    
    # TODO: Ideally TruthValue should be subclass of Value and thus                        
    # Value.__richcmp__() will be used to compare both. But TruthValue
    # was implemented earlier and is not subclass of Value.
    # This comparing procedure is workaround before proper fix.
    def compareWithTruthValue(self, other, op):
        cdef cTruthValue* self_ptr = <cTruthValue*>get_value_ptr(<Value>self)
        self_tv = TruthValue(deref(self_ptr).get_mean(),
                             deref(self_ptr).get_confidence())
        if op == Py_EQ:
            return self_tv == other
        elif op == Py_NE:
            return self_tv != other
        else:
            raise TypeError('Value can be compared using '
                            + 'Py_EQ and Py_NE only')

    def __richcmp__(self, other, op):
        if isinstance(other, TruthValue):
            return self.compareWithTruthValue(other, op)
        if not isinstance(other, Value):
            raise TypeError('Value cannot be compared with {}'
                            .format(type(other)))
        cdef cValue* self_ptr = (<Value>self).get_c_value_ptr().get()
        cdef cValue* other_ptr = (<Value>other).get_c_value_ptr().get()
        if op == Py_EQ:
            return deref(self_ptr) == deref(other_ptr)
        elif op == Py_NE:
            return deref(self_ptr) != deref(other_ptr)
        else:
            raise TypeError('Value can be compared using '
                            + 'Py_EQ and Py_NE only')
