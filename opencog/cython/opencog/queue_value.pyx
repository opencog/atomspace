from cython.operator cimport dereference as deref, preincrement as inc
from libcpp.vector cimport vector
from libcpp cimport bool

def createQueueValue(arg=None):
    cdef shared_ptr[cQueueValue] c_ptr
    if arg is None:
        c_ptr.reset(new cQueueValue())
    elif isinstance(arg, list):
        c_ptr.reset(new cQueueValue(QueueValue.list_of_values_to_vector(arg)))
    else:
        c_ptr.reset(new cQueueValue(QueueValue.list_of_values_to_vector([arg])))
    return QueueValue(PtrHolder.create(<shared_ptr[void]&>c_ptr))

cdef class QueueValue(Value):

    def open(self):
        """Open the queue for adding/removing values."""
        (<cQueueValue*>self.get_c_value_ptr().get()).open()

    def close(self):
        """Close the queue. No more values can be added after closing."""
        (<cQueueValue*>self.get_c_value_ptr().get()).close()

    def is_closed(self):
        """Check if the queue is closed."""
        return (<cQueueValue*>self.get_c_value_ptr().get()).is_closed()

    def clear(self):
        """Remove all values from the queue."""
        (<cQueueValue*>self.get_c_value_ptr().get()).clear()

    def push(self, Value value):
        """Add a value to the queue (same as add())."""
        (<cQueueValue*>self.get_c_value_ptr().get()).add(value.get_c_value_ptr())

    def pop(self):
        """Remove and return a value from the queue (same as remove()).

        Raises:
            RuntimeError: If the queue is closed and empty.
        """
        cdef cValuePtr c_value
        try:
            c_value = (<cQueueValue*>self.get_c_value_ptr().get()).remove()
            if c_value.get() == NULL:
                return None
            return create_python_value_from_c_value(c_value)
        except:
            # Convert C++ concurrent_queue::Canceled exception to Python RuntimeError
            # The C++ exception gets converted to a generic std::exception in Python
            raise RuntimeError("Cannot pop from closed empty queue")

    def append(self, Value value):
        """Add a value to the queue (Python list-like interface)."""
        self.push(value)

    def __len__(self):
        """Return the number of values in the queue."""
        return (<cQueueValue*>self.get_c_value_ptr().get()).size()

    def to_list(self):
        """Convert the queue contents to a Python list."""
        return QueueValue.vector_of_values_to_list(
            &((<cQueueValue*>self.get_c_value_ptr().get()).value()))

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