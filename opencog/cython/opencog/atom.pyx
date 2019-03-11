# Atom wrapper object
cdef class Atom(Value):

    @staticmethod
    cdef Atom createAtom(const cHandle& handle, AtomSpace a):
        return Atom(PtrHolder.create(<shared_ptr[void]&>handle), a)

    def __init__(self, ptr_holder, atomspace):
        super(Atom, self).__init__(ptr_holder)
        self.handle = <cHandle*>&((<PtrHolder>ptr_holder).shared_ptr)
        # cache the results after first retrieval of
        # immutable properties
        self._atom_type = None
        self._name = None
        self._outgoing = None
        self.atomspace = atomspace

    cdef cHandle get_c_handle(Atom self):
        """Return C++ shared_ptr from PtrHolder instance"""
        return <cHandle&>(self.ptr_holder.shared_ptr)

    def __nonzero__(self):
        """ Allows boolean comparison, return false is handle is
        UNDEFINED or doesn't exist in AtomSpace """
        if self.handle:
            return self.atomspace.is_valid(self)
        return False

    property atomspace:
        def __get__(self):
            return self.atomspace

    property name:
        def __get__(self):
            cdef cAtom* atom_ptr
            if self._name is None:
                atom_ptr = self.handle.atom_ptr()
                if atom_ptr == NULL:   # avoid null-pointer deref
                    return None
                if atom_ptr.is_node():
                    self._name = atom_ptr.get_name().decode('UTF-8')
                else:
                    self._name = ""
            return self._name

    property tv:
        def __get__(self):
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            cdef tv_ptr tvp
            if atom_ptr == NULL:   # avoid null-pointer deref
                return None
            tvp = atom_ptr.getTruthValue()
            if (not tvp.get()):
                raise AttributeError('cAtom returned NULL TruthValue pointer')
            return createTruthValue(tvp.get().get_mean(), tvp.get().get_confidence())

        def __set__(self, truth_value):
            try:
                assert isinstance(truth_value, TruthValue)
            except AssertionError:
                raise TypeError("atom.tv property needs a TruthValue object")
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return
            atom_ptr.setTruthValue(deref((<TruthValue>truth_value)._tvptr()))

    def set_value(self, key, value):
        self.get_c_handle().get().setValue(deref((<Atom>key).handle),
                                (<Value>value).get_c_value_ptr())

    def get_value(self, key):
        cdef cValuePtr value = self.get_c_handle().get().getValue(
            deref((<Atom>key).handle))
        if value.get() == NULL:
            return None
        return create_python_value_from_c_value(value, self.atomspace)

    def get_out(self):
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            return None
        cdef vector[cHandle] handle_vector = atom_ptr.getOutgoingSet()
        return convert_handle_seq_to_python_list(handle_vector, self.atomspace)

    property out:
        def __get__(self):
            if self._outgoing is None:
                atom_ptr = self.handle.atom_ptr()
                if atom_ptr == NULL:   # avoid null-pointer deref
                    return None
                if atom_ptr.is_link():
                    self._outgoing = self.get_out()
                else:
                    self._outgoing = []
            return self._outgoing

    property arity:
        def __get__(self):
            return len(self.out)

    property incoming:
        def __get__(self):
            cdef vector[cHandle] handle_vector
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return None
            atom_ptr.getIncomingSet(back_inserter(handle_vector))
            return convert_handle_seq_to_python_list(handle_vector, self.atomspace)

    def incoming_by_type(self, Type type):
        cdef vector[cHandle] handle_vector
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            return None
        atom_ptr.getIncomingSetByType(back_inserter(handle_vector), type)
        return convert_handle_seq_to_python_list(handle_vector, self.atomspace)

    def truth_value(self, mean, count):
        self.tv = createTruthValue(mean, count)
        return self

    def handle_ptr(self):
        return PyLong_FromVoidPtr(self.handle)

    def __richcmp__(a1_, a2_, int op):
        if not isinstance(a1_, Atom) or not isinstance(a2_, Atom):
            return NotImplemented
        cdef Atom a1 = a1_
        cdef Atom a2 = a2_

        is_equal = (a1.atomspace == a2.atomspace and
                     deref(a1.handle) == deref(a2.handle))
        if op == 2: # ==
            return is_equal
        elif op == 3: # !=
            return not is_equal

    # Necessary to prevent weirdness with RPyC
    def __cmp__(a1_, a2_):
        if not isinstance(a1_, Atom) or not isinstance(a2_, Atom):
            return NotImplemented
        cdef Atom a1 = a1_
        cdef Atom a2 = a2_
        is_equal = (a1.atomspace == a2.atomspace and
                     deref(a1.handle) == deref(a2.handle))
        if is_equal:
            return 0
        else:
            return -1

    def __hash__(a1):
        # Use the address of the atom in memory as the hash.
        # This should be globally unique, because the atomspace
        # does not allow more than one, ever.
        return hash(PyLong_FromVoidPtr(a1.handle.atom_ptr()))
