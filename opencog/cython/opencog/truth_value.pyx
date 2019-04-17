from cython.operator cimport dereference as deref
from libcpp.memory cimport shared_ptr
from atomspace cimport cTruthValue, cSimpleTruthValue, tv_ptr, Value

def createTruthValue(strength = 1.0, confidence = 1.0):
    cdef tv_ptr c_ptr
    c_ptr.reset(new cSimpleTruthValue(strength, confidence))
    return TruthValue(ptr_holder = PtrHolder.create(<shared_ptr[void]&>c_ptr))


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
    def __init__(self, strength=1.0, confidence=1.0, ptr_holder = None):
        cdef tv_ptr c_ptr
        if ptr_holder is not None:
            super(TruthValue, self).__init__(ptr_holder)
        else:
            c_ptr.reset(new cSimpleTruthValue(strength, confidence))
            super(TruthValue, self).__init__(PtrHolder.create(<shared_ptr[void]&>c_ptr))

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


class SimpleTruthValue(TruthValue):
    pass


MEAN = 0
CONFIDENCE = 1

try:
    import torch
    class TensorTruthValueWrapper(torch.Tensor):

        @staticmethod
        def __new__(cls, *args):
           if len(args) == 1:
               assert(len(args[0]) == 2)
               instance = super().__new__(cls, *args)
           elif len(args) == 2:
               instance = super().__new__(cls, args)
           else:
               raise RuntimeError("Expecting tuple of two number, \
                       tensor of len 2 or two numbers, got {0}".format(args))
           return instance

        @property
        def mean(self):
            return self[MEAN]

        @property
        def confidence(self):
            return self[CONFIDENCE]

        def __str__(self):
            return 'TensorTruthValue({0}, {1})'.format(self.mean,
                                                       self.confidence)


except ImportError as e:
    print("Torch not found, torch truth value will not be available")


cdef class TensorTruthValue(TruthValue):
    cdef object ttv
    def __init__(self, *args, **kwargs):
        cdef tv_ptr c_ptr
        cdef cTensorTruthValue * this_ptr
        ptr_holder = kwargs.get('ptr_holder', None)
        if ptr_holder is not None:
            super(TruthValue, self).__init__(ptr_holder=ptr_holder)
            this_ptr = <cTensorTruthValue*>self.get_c_value_ptr().get()
            self.ttv = <object>(deref(this_ptr).getPtr())
        else:
            self.ttv = TensorTruthValueWrapper(*args)
            c_ptr = <tv_ptr>createTensorTruthValue(<PyObject*>self.ttv)
            super(TruthValue, self).__init__(PtrHolder.create(<shared_ptr[void]&>c_ptr))

    cdef _mean(self):
        return self.ttv.mean

    cdef _confidence(self):
        return self.ttv.confidence

    def torch(self):
        return self.ttv

    def __getitem__(self, idx):
        assert 0 <= idx <= 1
        return self.ttv[idx]

    def __str__(self):
        return str(self.ttv)
