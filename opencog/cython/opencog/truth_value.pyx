from cython.operator cimport dereference as deref
from libcpp.memory cimport shared_ptr
from atomspace cimport cTruthValue, cSimpleTruthValue, tv_ptr, Value

cdef class TruthValue(Value):
    """ The truth value represents the strength and confidence of
        a relationship or term. In OpenCog there are a number of TruthValue
        types, but as these involve additional complexity we focus primarily on
        the SimpleTruthValue type which allows strength and count

        @todo Support IndefiniteTruthValue, DistributionalTV, NullTV etc
    """
    def __init__(self, strength=1.0, confidence=0.0):
        cdef tv_ptr c_ptr
        c_ptr.reset(new cSimpleTruthValue(strength, confidence))
        super(TruthValue, self).__init__(ValuePtr.create(<cValuePtr&>c_ptr))

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

    def __richcmp__(TruthValue h1, TruthValue h2, int op):
        " @todo support the rest of the comparison operators"
        if op == 2: # ==
            return deref(h1._ptr()) == deref(h2._ptr())

        raise ValueError, "TruthValue does not yet support most comparison operators"

    cdef cTruthValue* _ptr(self):
        return <cTruthValue*>(self.get_c_value_ptr().get())

    cdef tv_ptr* _tvptr(self):
        return <tv_ptr*>&(self.value_ptr.shared_ptr)

    def truth_value_ptr_object(self):
        return PyLong_FromVoidPtr(<void*>self._tvptr())

    def __str__(self):
        cs = string(self._ptr().to_string().c_str())
        return cs.decode('UTF-8')

    def __repr__(self):
        cs = string(self._ptr().to_string().c_str())
        return cs.decode('UTF-8')

#    @staticmethod
#    def confidence_to_count(double conf):
#        return (<cSimpleTruthValue*> 0).confidenceToCount(conf)
#
#    @staticmethod
#    def count_to_confidence(double count):
#        return (<cSimpleTruthValue*> 0).countToConfidence(count)
