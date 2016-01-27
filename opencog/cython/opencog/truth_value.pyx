from cython.operator cimport dereference as deref

from atomspace cimport cTruthValue, tv_ptr

cdef class TruthValue:
    """ The truth value represents the strength and confidence of
        a relationship or term. In OpenCog there are a number of TruthValue
        types, but as these involve additional complexity we focus primarily on
        the SimpleTruthValue type which allows strength and count

        @todo Support IndefiniteTruthValue, DistributionalTV, NullTV etc
    """
    # This stores a pointer to a smart pointer to the C++ TruthValue object
    # Declared in atomspace.pxd
    # cdef tv_ptr *cobj

    def __cinit__(self, strength=1.0, confidence=0.0):
        # By default create a SimpleTruthValue
        self.cobj = new tv_ptr(new cSimpleTruthValue(strength, self.confidence_to_count(confidence))) 

    def __dealloc__(self):
        # This deletes the *smart pointer*, not the actual pointer
        del self.cobj

    property mean:
        def __get__(self): return self._mean()

    property confidence:
        def __get__(self): return self._confidence()

    property count:
        def __get__(self): return self._count()

    cdef _mean(self):
        return self._ptr().getMean()

    cdef _confidence(self):
        return self._ptr().getConfidence()

    cdef _count(self):
        return self._ptr().getCount()

    cdef _init(self, float mean, float confidence):
        deref((<cSimpleTruthValue*>self._ptr())).initialize(mean, self.confidence_to_count(confidence))

    def set_value(self, mean, confidence):
        self._init(mean, confidence)

    def __richcmp__(TruthValue h1, TruthValue h2, int op):
        " @todo support the rest of the comparison operators"
        if op == 2: # ==
            return deref(h1._ptr()) == deref(h2._ptr())
        
        raise ValueError, "TruthValue does not yet support most comparison operators"

    cdef cTruthValue* _ptr(self):
        return self.cobj.get()

    cdef tv_ptr* _tvptr(self):
        return self.cobj

    def truth_value_ptr_object(self):
        return PyLong_FromVoidPtr(<void*>self.cobj)

    def __str__(self):
        return self._ptr().toString().c_str()

    @staticmethod
    def confidence_to_count(float conf):
        return (<cSimpleTruthValue*> 0).confidenceToCount(conf)

    @staticmethod
    def count_to_confidence(float count):
        return (<cSimpleTruthValue*> 0).countToConfidence(count)
