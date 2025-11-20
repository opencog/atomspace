from cpython.object cimport Py_EQ, Py_NE
from cython.operator cimport dereference as deref, preincrement as inc


cdef class Value:

    @staticmethod
    cdef Value create(cValuePtr& ptr):
        """Factory method to construct Value from C++ cValuePtr.

        Uses __new__ to bypass __init__, allowing direct assignment of C++ shared_ptr.
        """
        # Well ... We can get null pointers, because asking for
        # a value on an atom at a non-existant key weill return
        # nullptr. So, currently, null pointers do occur.
        # if ptr.get() == NULL:
        #     raise RuntimeError("Value pointer cannot be null!")

        cdef Value instance = Value.__new__(Value)
        instance.shared_ptr = ptr
        return instance

    def __init__(self):
        """Default constructor - creates empty/null Value.

        To create Value from C++ pointer, use Value.create() factory method.
        """
        # Leave shared_ptr uninitialized (will be null)
        pass

    cdef inline cValuePtr get_c_value_ptr(self) nogil:
        """Return C++ shared_ptr directly"""
        return self.shared_ptr

    cdef inline cValue* get_c_raw_ptr(self) nogil:
        """Return C++ raw pointer"""
        return self.shared_ptr.get()

    @property
    def type(self):
        return self.get_c_raw_ptr().get_type()

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

    def __iter__(self):
        return self.to_list().__iter__()

    def long_string(self):
        return self.get_c_raw_ptr().to_string().decode('UTF-8')

    def short_string(self):
        return self.get_c_raw_ptr().to_short_string().decode('UTF-8')

    # long_string() provides the atom, together with the hash, and
    # the AtomSpace that the atom belongs to. This is perhaps more
    # than what the typical python user might want. So use the short
    # string when printing.  This still prints in scheme format, BTW ...
    def __str__(self):
        if self.is_atom():
           return self.short_string()
        return self.long_string()

    def __repr__(self):
        if self.is_atom():
           return self.short_string()
        return self.long_string()

    def __richcmp__(self, other, op):
        if not isinstance(other, Value):
            # raise TypeError('Value cannot be compared with {}'
            #                 .format(type(other)))
            if op == Py_EQ:
                return False
            if op == Py_NE:
                return True

        cdef cValue* self_ptr = (<Value>self).get_c_raw_ptr()
        cdef cValue* other_ptr = (<Value>other).get_c_raw_ptr()
        if op == Py_EQ:
            return deref(self_ptr) == deref(other_ptr)
        if op == Py_NE:
            return deref(self_ptr) != deref(other_ptr)

        raise TypeError('Value can be compared using '
                        + 'Py_EQ and Py_NE only')
