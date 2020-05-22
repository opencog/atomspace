from cython.operator cimport dereference as deref
from libcpp.memory cimport shared_ptr
# from atomspace cimport cTruthValue, cSimpleTruthValue, tv_ptr, TruthValue, Value

def createTruthValue(strength = 1.0, confidence = 1.0):
    cdef tv_ptr c_ptr
    c_ptr.reset(new cSimpleTruthValue(strength, confidence))
    return TruthValue(ptr_holder=PtrHolder.create(<shared_ptr[void]&>c_ptr))

cdef class TruthValue(Value):
    """ The truth value represents the strength and confidence of
        a relationship or term. In OpenCog there are a number of TruthValue
        types, but as these involve additional complexity we focus primarily on
        the SimpleTruthValue type which allows strength and count

        @todo Support IndefiniteTruthValue, DistributionalTV, NullTV etc
    """
    # Type constructors for all atoms and values are exported via
    # opencog.type_constructors module. Except TruthValue which is historically
    # exported in opencog.atomspace. To keep it work before proper fix
    # TruthValue constructor is modified to accept both old parameters
    # (strength and confidence) and new ptr_holder parameter.
    # def __init__(self, strength_t strength=1.0, confidence_t confidence=1.0, PtrHolder ptr_holder=None):
    #     cdef tv_ptr c_ptr
    #     if ptr_holder is not None:
    #         super().__init__(ptr_holder)
    #     else:
    #         c_ptr.reset(new cSimpleTruthValue(strength, confidence))
    #         super().__init__(PtrHolder.create(<shared_ptr[void]&>c_ptr))

    @property
    def mean(self):
        return self._mean()

    @property
    def confidence(self):
        return self._confidence()

    @property
    def count(self):
        return self._count()

    cdef strength_t _mean(self):
        return self._ptr().get_mean()

    cdef confidence_t _confidence(self):
        return self._ptr().get_confidence()

    cdef count_t _count(self):
        return self._ptr().get_count()

    cdef cTruthValue* _ptr(self):
        return <cTruthValue*>(self.get_c_value_ptr().get())

    cdef tv_ptr* _tvptr(self):
        return <tv_ptr*>&((<PtrHolder>self.ptr_holder).shared_ptr)
