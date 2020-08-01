from cpython.object cimport Py_EQ, Py_NE
from cython.operator cimport dereference as deref


cdef class PtrHolder:
    """C++ shared_ptr object wrapper for Python clients. Cython cannot create
    Python object constructor which gets C++ pointer. This class is used to
    wrap pointer and make it possible to initialize Value in usual
    constructor (see
    http://docs.cython.org/en/latest/src/userguide/extension_types.html#instantiation-from-existing-c-c-pointers)."""

    @staticmethod
    cdef PtrHolder create(shared_ptr[void]& ptr):
        """Factory method to construct PtrHolder from C++ shared_ptr"""
        cdef PtrHolder ptr_holder = PtrHolder.__new__(PtrHolder)
        ptr_holder.shared_ptr = ptr
        return ptr_holder

cdef class Value:

    @staticmethod
    cdef Value create(cValuePtr& ptr):
        """Factory method to construct Value from C++ cValuePtr using
        PtrHolder instance."""
        return Value(PtrHolder.create(<shared_ptr[void]&>ptr))

    def __init__(self, ptr_holder):
        if (<PtrHolder>ptr_holder).shared_ptr.get() == NULL:
            raise AttributeError('PtrHolder contains NULL reference')
        self.ptr_holder = ptr_holder

    cdef cValuePtr get_c_value_ptr(self):
        """Return C++ shared_ptr from PtrHolder instance"""
        return <cValuePtr&>(self.ptr_holder.shared_ptr)

    def value_ptr(self):
        return PyLong_FromVoidPtr(<cValuePtr*>&(self.ptr_holder.shared_ptr))

    @property
    def type(self):
        return self.get_c_value_ptr().get().get_type()

    @property
    def type_name(self):
        return get_type_name(self.type)

    def is_atom(self):
        return is_a(self.type, types.Atom)

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
        return self.long_string()

    def __repr__(self):
        return self.long_string()

    def __richcmp__(self, other, op):
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
