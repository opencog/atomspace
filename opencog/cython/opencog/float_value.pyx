
def createFloatValue(arg):
    cdef shared_ptr[cFloatValue] c_ptr
    if (isinstance(arg, list)):
        c_ptr.reset(new cFloatValue(FloatValue.list_of_doubles_to_vector(arg)))
    else:
        c_ptr.reset(new cFloatValue(<double>arg))
    return FloatValue(PtrHolder.create(<shared_ptr[void]&>c_ptr))

cdef class FloatValue(Value):

    def to_list(self):
        return FloatValue.vector_of_doubles_to_list(
            &((<cFloatValue*>self.get_c_value_ptr().get()).value()))

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

