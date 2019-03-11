from opencog.atomspace import types
from cython.operator cimport dereference as deref, preincrement as inc
from opencog.atomspace cimport cHandle, Atom, AtomSpace, TruthValue
from ure cimport cForwardChainer

# Create a Cython extension type which holds a C++ instance
# as an attribute and create a bunch of forwarding methods
# Python extension type.


cdef class ForwardChainer:
    cdef cForwardChainer * chainer
    cdef AtomSpace _as
    def __cinit__(self, AtomSpace _as,
                  Atom rbs,
                  Atom source,
                  Atom vardecl=None,
                  focus_set=[]):
        cdef cHandle c_vardecl
        if vardecl is None:
            c_vardecl = c_vardecl.UNDEFINED
        else:
            c_vardecl = deref(vardecl.handle)

        cdef vector[cHandle] handle_vector
        for atom in focus_set:
            if isinstance(atom, Atom):
                handle_vector.push_back(deref((<Atom>(atom)).handle))

        self.chainer = new cForwardChainer(deref(_as.atomspace),
                                        deref(rbs.atomspace.atomspace),
                                        deref(rbs.handle),
                                        deref(source.handle),
                                        c_vardecl,
                                        handle_vector)
        self._as = _as

    def do_chain(self):
        return self.chainer.do_chain()

    def get_results(self):
        cdef set[cHandle] res_handle_set = self.chainer.get_chaining_result()
        list = []
        cdef set[cHandle].iterator it = res_handle_set.begin()
        while it != res_handle_set.end():
            handle = deref(it)
            list.append(Atom.createAtom(handle, self._as))
            inc(it)

        result_set = self._as.add_link(types.SetLink, list)
        return result_set

    def __dealloc__(self):
        del self.chainer
        self._as = None
