cdef class FloatValue(Value):
    @staticmethod
    cdef vector[double] list_of_doubles_to_vector(list python_list)

    @staticmethod
    cdef list vector_of_doubles_to_list(const vector[double]* cpp_vector)


cdef class StringValue(Value):
    @staticmethod
    cdef vector[string] list_of_strings_to_vector(list python_list)

    @staticmethod
    cdef list vector_of_strings_to_list(const vector[string]* cpp_vector)


cdef class LinkValue(Value):
    @staticmethod
    cdef vector[cValuePtr] list_of_values_to_vector(list python_list)

    @staticmethod
    cdef list vector_of_values_to_list(const vector[cValuePtr]* cpp_vector)
