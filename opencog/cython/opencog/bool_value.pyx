
def createBoolValue(arg):
    cdef shared_ptr[cBoolValue] c_ptr
    if (isinstance(arg, list)):
        c_ptr = c_createBoolValue_vector(BoolValue.list_of_bool_to_vector(arg))
    else:
        c_ptr = c_createBoolValue_single(<bool>arg)
    cdef BoolValue instance = BoolValue.__new__(BoolValue)
    instance.shared_ptr = <cValuePtr&>(c_ptr, c_ptr.get())
    return instance

cdef class BoolValue(Value):

    def to_list(self):
        return BoolValue.vector_of_bool_to_list(
            (<cBoolValue*>self.get_c_raw_ptr()).value())

    @staticmethod
    cdef vector[bool] list_of_bool_to_vector(list python_list):
        cdef vector[bool] cpp_vector
        cdef bool value
        for value in python_list:
            cpp_vector.push_back(value)
        return cpp_vector

    @staticmethod
    cdef list vector_of_bool_to_list(vector[bool] cpp_vector):
        list = []
        it = cpp_vector.const_begin()
        while it != cpp_vector.const_end():
            list.append(deref(it))
            inc(it)
        return list
