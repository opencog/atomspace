from cpython cimport PyLong_FromLongLong
from cpython.object cimport Py_LT, Py_EQ, Py_GT, Py_LE, Py_NE, Py_GE
from libcpp.set cimport set as cpp_set

# from atomspace cimport Atom

# Atom wrapper object
cdef class Atom(Value):

    def __cinit__(self, PtrHolder ptr_holder, *args, **kwargs):
        self.handle = <cHandle*>&((<PtrHolder>ptr_holder).shared_ptr)
        self._atom_type = None
        self._name = None
        self._outgoing = None
    
    @staticmethod
    cdef Atom createAtom(const cHandle& handle):
        return Atom(PtrHolder.create(<shared_ptr[void]&>handle))

    cdef cHandle get_c_handle(Atom self):
        """Return C++ shared_ptr from PtrHolder instance"""
        return <cHandle&>(self.ptr_holder.shared_ptr)

    @property
    def atomspace(self):
        cdef cAtomSpace* a = self.get_c_handle().get().getAtomSpace()
        return AtomSpace_factoid(as_cast(a))

    @property
    def name(self):
        cdef cAtom* atom_ptr
        if self._name is None:
            atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                raise RuntimeError("Null Atom!")
            if atom_ptr.is_node():
                self._name = atom_ptr.get_name().decode('UTF-8')
            else:
                self._name = ""
        return self._name

    @property
    def tv(self):
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        cdef tv_ptr tvp
        if atom_ptr == NULL:   # avoid null-pointer deref
            raise RuntimeError("Null Atom!")
        tvp = atom_ptr.getTruthValue()
        if (not tvp.get()):
            raise AttributeError('cAtom returned NULL TruthValue pointer')
        return createTruthValue(tvp.get().get_mean(), tvp.get().get_confidence())

    @tv.setter
    def tv(self, truth_value):
        try:
            assert isinstance(truth_value, TruthValue)
        except AssertionError:
            raise TypeError("atom.tv property needs a TruthValue object")
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            raise RuntimeError("Null Atom!")
        atom_ptr.setTruthValue(deref((<TruthValue>truth_value)._tvptr()))

    def id_string(self):
        return self.get_c_handle().get().id_to_string().decode('UTF-8')

    def set_value(self, key, value):
        if not isinstance(key, Atom):
            raise TypeError("key should be an instance of Atom, got {0} instead".format(type(key)))
        self.get_c_handle().get().setValue(deref((<Atom>key).handle),
                                (<Value>value).get_c_value_ptr())

    def get_value(self, key):
        cdef cValuePtr value = self.get_c_handle().get().getValue(
            deref((<Atom>key).handle))
        if value.get() == NULL:
            raise RuntimeError("Null Atom!")
        return create_python_value_from_c_value(value)

    def get_keys(self):
        """
        Returns the keys of values associated with this atom.

        :returns: A list of atoms.
        """
        cdef cpp_set[cHandle] keys = self.get_c_handle().get().getKeys()
        return convert_handle_set_to_python_list(keys)

    def get_out(self):
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            raise RuntimeError("Null Atom!")
        cdef vector[cHandle] handle_vector = atom_ptr.getOutgoingSet()
        return convert_handle_seq_to_python_list(handle_vector)

    def to_list(self):
        if self._outgoing is None:
            atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                raise RuntimeError("Null Atom!")
            if atom_ptr.is_link():
                self._outgoing = self.get_out()
            else:
                self._outgoing = []
        return self._outgoing

    @property
    def out(self):
        return self.to_list()

    @property
    def arity(self):
        return len(self.out)

    @property
    def incoming(self):
        cdef vector[cHandle] handle_vector
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            raise RuntimeError("Null Atom!")
        atom_ptr.getIncomingIter(back_inserter(handle_vector))
        return convert_handle_seq_to_python_list(handle_vector)

    def incoming_by_type(self, Type type):
        cdef vector[cHandle] handle_vector
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            raise RuntimeError("Null Atom!")
        atom_ptr.getIncomingSetByType(back_inserter(handle_vector), type)
        return convert_handle_seq_to_python_list(handle_vector)

    def truth_value(self, mean, count):
        self.tv = createTruthValue(mean, count)
        return self

    def __richcmp__(self, other, int op):
        assert isinstance(other, Atom), "Only Atom instances are comparable with atoms"
        if op == Py_LT:
            return self.__lt(other)
        if op == Py_EQ:
            return self.__eq(other)
        if op == Py_GT:
            return other.__lt(self)
        if op == Py_LE:
            return not other.__lt(self)
        if op == Py_NE:
            return not self.__eq(other)
        if op == Py_GE:
            return not self.__lt(other)
        raise RuntimeError("unexpected comparison kind: {0}".format(op))

    def __lt(self, other):
        assert isinstance(other, Atom), "Only Atom instances are comparable with atoms"
        cdef cAtom* p = self.get_c_handle().get()
        cdef cAtom* o = ((<Atom>other).get_c_handle()).get()
        return deref(p) < deref(o)

    def __eq(self, other):
        if not isinstance(other, Atom):
            return False
        cdef cAtom* p = self.get_c_handle().get()
        cdef cAtom* o = (<Atom>other).get_c_handle().get()
        return deref(p) == deref(o)

    def __hash__(self):
        return PyLong_FromLongLong(self.get_c_handle().get().get_hash())
