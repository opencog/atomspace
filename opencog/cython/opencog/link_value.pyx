
def createLinkValue(arg):
    cdef shared_ptr[cLinkValue] c_ptr
    if (isinstance(arg, list)):
        c_ptr.reset(new cLinkValue(LinkValue.list_of_values_to_vector(arg)))
    else:
        c_ptr.reset(new cLinkValue(LinkValue.list_of_values_to_vector([arg])))
    return LinkValue(PtrHolder.create(<shared_ptr[void]&>c_ptr))

cdef class LinkValue(Value):

    def to_list(self):
        return LinkValue.vector_of_values_to_list(
            &((<cLinkValue*>self.get_c_value_ptr().get()).value()))

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
