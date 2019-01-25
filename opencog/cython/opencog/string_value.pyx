
cdef class StringValue(Value):

    def __init__(self, arg):
        cdef cValuePtr result
        if (isinstance(arg, list)):
            result = createStringValue(StringValue.list_of_strings_to_vector(arg))
        else:
            result = createStringValue(<string>(arg.encode('UTF-8')))
        super(StringValue, self).__init__(ValuePtr.create(result))

    def to_list(self):
        return StringValue.vector_of_strings_to_list(
            &((<cStringValue*>self.get_c_value_ptr().get()).value()))

    @staticmethod
    cdef vector[string] list_of_strings_to_vector(list python_list):
        cdef vector[string] cpp_vector
        for value in python_list:
            cpp_vector.push_back(value.encode('UTF-8'))
        return cpp_vector

    @staticmethod
    cdef list vector_of_strings_to_list(const vector[string]* cpp_vector):
        list = []
        it = cpp_vector.const_begin()
        while it != cpp_vector.const_end():
            list.append((<bytes>deref(it).c_str()).decode('UTF-8'))
            inc(it)
        return list
