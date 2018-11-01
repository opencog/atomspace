from libcpp.set cimport set
from cython.operator cimport dereference as deref
from opencog.atomspace cimport Atom
from opencog.atomspace cimport void_from_candle
from opencog.atomspace cimport cHandle, AtomSpace, TruthValue
from opencog.atomspace cimport void_from_candle
from opencog.atomspace import types
from backwardchainer cimport cBackwardChainer

# Create a Cython extension type which holds a C++ instance
# as an attribute and create a bunch of forwarding methods
# Python extension type.


cdef class BackwardChainer:
    cdef cBackwardChainer * chainer
    cdef AtomSpace _as
# scheme interface
#    (define* (cog-bc rbs target
#                 #:key
#                 (vardecl (List)) (trace-as #f) (control-as #f) (focus-set (Set)))
    def __cinit__(self, AtomSpace _as,
                  Atom rbs,
                  Atom target,
                  Atom vardecl=None,
                  AtomSpace trace_as=None,
                  AtomSpace control_as=None,
                  Atom focus_set=None):

        if vardecl is None:
            vardecl = _as.add_link(types.VariableList, [])
        if focus_set is None:
            focus_set = _as.add_link(types.SetLink, [])
        self.chainer = new cBackwardChainer(deref(_as.atomspace),
                                        deref(rbs.handle),
                                        deref(target.handle),
                                        deref(vardecl.handle),
                                        <cAtomSpace*> (NULL if trace_as is None else trace_as.atomspace),
                                        <cAtomSpace*> (NULL if control_as is None else control_as.atomspace),
                                        deref(focus_set.handle))
        self._as = _as

    def do_chain(self):
        return self.chainer.do_chain()

    def get_results(self):
        cdef cHandle res_handle = self.chainer.get_results()
        cdef Atom result = Atom(void_from_candle(res_handle), self._as)
        return result

