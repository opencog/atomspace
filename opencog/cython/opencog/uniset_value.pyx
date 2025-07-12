from cython.operator cimport dereference as deref, preincrement as inc
from libcpp.vector cimport vector
from libcpp cimport bool

def createUnisetValue(arg=None):
    cdef shared_ptr[cUnisetValue] c_ptr
    if arg is None:
        c_ptr.reset(new cUnisetValue())
    elif isinstance(arg, list):
        c_ptr.reset(new cUnisetValue(UnisetValue.list_of_values_to_vector(arg)))
    else:
        c_ptr.reset(new cUnisetValue(UnisetValue.list_of_values_to_vector([arg])))
    return UnisetValue(PtrHolder.create(<shared_ptr[void]&>c_ptr))

cdef class UnisetValue(Value):

    def open(self):
        """Open the set for adding/removing values."""
        cdef cUnisetValue* set_ptr = <cUnisetValue*>self.get_c_value_ptr().get()
        with nogil:
            set_ptr.open()

    def close(self):
        """Close the set. No more values can be added after closing."""
        cdef cUnisetValue* set_ptr = <cUnisetValue*>self.get_c_value_ptr().get()
        with nogil:
            set_ptr.close()

    def is_closed(self):
        """Check if the set is closed."""
        cdef cUnisetValue* set_ptr = <cUnisetValue*>self.get_c_value_ptr().get()
        cdef bool result
        with nogil:
            result = set_ptr.is_closed()
        return result

    def clear(self):
        """Remove all values from the set."""
        cdef cUnisetValue* set_ptr = <cUnisetValue*>self.get_c_value_ptr().get()
        with nogil:
            set_ptr.clear()

    def add(self, Value value):
        """Add a value to the set (duplicates are ignored)."""
        cdef cValuePtr val_ptr = value.get_c_value_ptr()
        cdef cUnisetValue* set_ptr = <cUnisetValue*>self.get_c_value_ptr().get()

        # Release the GIL since the C++ method is thread-safe
        with nogil:
            set_ptr.add(val_ptr)

    def push(self, Value value):
        """Add a value to the set (same as add())."""
        self.add(value)

    def pop(self):
        """Remove and return a value from the set (same as remove()).

        Raises:
            RuntimeError: If the set is closed and empty.
        """
        return self.remove()

    def remove(self):
        """Remove and return a value from the set.

        Raises:
            RuntimeError: If the set is closed and empty.
        """
        cdef cValuePtr c_value
        cdef cUnisetValue* set_ptr = <cUnisetValue*>self.get_c_value_ptr().get()

        # Release the GIL while waiting for values, so other Python threads can run
        try:
            with nogil:
                c_value = set_ptr.remove()
        except:
            # Convert any exception from remove() to our RuntimeError
            raise RuntimeError("Cannot remove from closed empty set")

        if c_value.get() == NULL:
            return None
        return create_python_value_from_c_value(c_value)

    def append(self, Value value):
        """Add a value to the set (Python list-like interface)."""
        self.add(value)

    def __len__(self):
        """Return the number of values in the set."""
        cdef cUnisetValue* set_ptr = <cUnisetValue*>self.get_c_value_ptr().get()
        cdef size_t result
        with nogil:
            result = set_ptr.size()
        return result

    def to_list(self):
        """Convert the set contents to a Python list."""
        return UnisetValue.vector_of_values_to_list(
            &((<cUnisetValue*>self.get_c_value_ptr().get()).value()))

    @staticmethod
    cdef vector[cValuePtr] list_of_values_to_vector(list python_list):
        cdef vector[cValuePtr] cpp_vector
        cdef Value value
        for value in python_list:
            cpp_vector.push_back(value.get_c_value_ptr())
        return cpp_vector

    @staticmethod
    cdef list vector_of_values_to_list(const vector[cValuePtr]* cpp_vector):
        list = []
        it = cpp_vector.const_begin()
        cdef cValuePtr value
        while it != cpp_vector.const_end():
            value = deref(it)
            list.append(create_python_value_from_c_value(value))
            inc(it)
        return list