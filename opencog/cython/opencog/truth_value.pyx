from cython.operator cimport dereference as deref
from libcpp.memory cimport shared_ptr
from atomspace cimport cTruthValue, cSimpleTruthValue, tv_ptr, Value

def createTruthValue(strength = 1.0, confidence = 1.0):
    cdef tv_ptr c_ptr
    c_ptr.reset(new cSimpleTruthValue(strength, confidence))
    return TruthValue(PtrHolder.create(<shared_ptr[void]&>c_ptr))

cdef class TruthValue(Value):
    """ The truth value represents the strength and confidence of
        a relationship or term. In OpenCog there are a number of TruthValue
        types, but as these involve additional complexity we focus primarily on
        the SimpleTruthValue type which allows strength and count

        @todo Support IndefiniteTruthValue, DistributionalTV, NullTV etc
    """
    def __init__(self, ptr_holder):
        super(TruthValue, self).__init__(ptr_holder)

    property mean:
        def __get__(self): return self._mean()

    property confidence:
        def __get__(self): return self._confidence()

    property count:
        def __get__(self): return self._count()

    cdef _mean(self):
        return self._ptr().get_mean()

    cdef _confidence(self):
        return self._ptr().get_confidence()

    cdef _count(self):
        return self._ptr().get_count()

    cdef cTruthValue* _ptr(self):
        return <cTruthValue*>(self.get_c_value_ptr().get())

    cdef tv_ptr* _tvptr(self):
        return <tv_ptr*>&(self.ptr_holder.shared_ptr)

    def truth_value_ptr_object(self):
        return PyLong_FromVoidPtr(<void*>self._tvptr())

