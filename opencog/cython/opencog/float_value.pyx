
cdef class FloatValue(Value):

    def __init__(self, arg):
        cdef cValuePtr result
        if (isinstance(arg, list)):
            result = createFloatValue(FloatValue.list_of_doubles_to_vector(arg))
        else:
            result = createFloatValue(<double>arg)
        super(FloatValue, self).__init__(ValuePtr.create(result))

    def to_list(self):
        return FloatValue.vector_of_doubles_to_list(
            &((<cFloatValue*>self.get_c_value()).value()))

    @staticmethod
    cdef vector[double] list_of_doubles_to_vector(list python_list):
        cdef vector[double] cpp_vector
        cdef double value
        for value in python_list:
            cpp_vector.push_back(value)
        return cpp_vector

    @staticmethod
    cdef list vector_of_doubles_to_list(const vector[double]* cpp_vector):
        list = []
        it = cpp_vector.const_begin()
        while it != cpp_vector.const_end():
            list.append(deref(it))
            inc(it)
        return list

