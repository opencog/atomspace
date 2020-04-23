
def createStringValue(arg):
    cdef shared_ptr[cStringValue] c_ptr
    if (isinstance(arg, list)):
        c_ptr.reset(new cStringValue(StringValue.list_of_strings_to_vector(arg)))
    else:
        c_ptr.reset(new cStringValue(<string>(arg.encode('UTF-8'))))
    return StringValue(PtrHolder.create(<shared_ptr[void]&>c_ptr))

cdef class StringValue(Value):

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
