
cdef cAtom* get_atom_ptr(Atom atom):
    cdef cAtom* atom_ptr = atom.handle.atom_ptr()
    if atom_ptr == NULL:
        raise AttributeError('Atom contains NULL reference')
    return atom_ptr


# Atom wrapper object
cdef class Atom(object):

    def __cinit__(self, PANDLE lptr, AtomSpace a):
        atomo = atom_from_the_void(lptr)
        self.handle = new cHandle(atomo)

    def __dealloc__(self):
        del self.handle

    def __init__(self, PANDLE lptr, AtomSpace a):
        # self.handle = h is set in __cinit__ above

        # cache the results after first retrieval of
        # immutable properties
        self._atom_type = None
        self._name = None
        self._outgoing = None
        self.atomspace = a

    def __nonzero__(self):
        """ Allows boolean comparison, return false is handle is
        UNDEFINED or doesn't exist in AtomSpace """
        if self.handle:
            return self.atomspace.is_valid(self)
        else: return False

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
                pytv = TruthValue()
                pytv.cobj = new tv_ptr(tvp) # make copy of smart pointer
                return pytv
            return TruthValue(tvp.get().get_mean(), tvp.get().get_confidence())

        def __set__(self, truth_value):
            try:
                assert isinstance(truth_value, TruthValue)
            except AssertionError:
                raise TypeError("atom.av property needs a TruthValue object")
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return
            atom_ptr.setTruthValue(deref((<TruthValue>truth_value)._tvptr()))

    property av:
        def __get__(self):
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return None
            sti = get_sti(self.handle[0])
            lti = get_lti(self.handle[0])
            vlti = get_vlti(self.handle[0])
            return { "sti": sti, "lti": lti, "vlti": vlti }
        def __set__(self, av_dict):
            try:
                assert isinstance(av_dict, dict)
            except AssertionError:
                raise TypeError("atom.av property needs a dictionary object")
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return
            if av_dict:
                if "sti" in av_dict: sti = av_dict["sti"]
                if "lti" in av_dict: lti = av_dict["lti"]
                if "vlti" in av_dict: vlti = av_dict["vlti"]
            if sti:
                attentionbank(self.atomspace.atomspace).set_sti(self.handle[0], sti)
            if lti:
                attentionbank(self.atomspace.atomspace).set_lti(self.handle[0], lti)
            if vlti:
                vlti = vlti - get_vlti(self.handle[0])
                if vlti > 0:
                    while vlti > 0:
                        self.increment_vlti()
                        vlti = vlti - 1
                if vlti < 0:
                    while vlti < 0:
                        self.decrement_vlti()
                        vlti = vlti + 1

    property sti:
        def __get__(self):
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return None
            return get_sti(self.handle[0])
        def __set__(self,val):
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return
            attentionbank(self.atomspace.atomspace).set_sti(self.handle[0], val)

    property lti:
        def __get__(self):
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return None
            return get_lti(self.handle[0])
        def __set__(self,val):
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return
            attentionbank(self.atomspace.atomspace).set_lti(self.handle[0], val)

    property vlti:
        def __get__(self):
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return None
            return get_vlti(self.handle[0])
        def __set__(self, val):
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return
            vlti = val - get_vlti(self.handle[0])
            if vlti > 0:
                while vlti > 0:
                    self.increment_vlti()
                    vlti = vlti - 1
            if vlti < 0:
                while vlti < 0:
                    self.decrement_vlti()
                    vlti = vlti + 1

    def increment_vlti(self):
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            return
        attentionbank(self.atomspace.atomspace).inc_vlti(self.handle[0])

    def decrement_vlti(self):
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            return
        attentionbank(self.atomspace.atomspace).dec_vlti(self.handle[0])

    def set_value(self, key, value):
        get_atom_ptr(self).setValue(deref((<Atom>key).handle),
                                (<ProtoAtom>value).shared_ptr)
        
    def get_value(self, key):
        cdef cProtoAtomPtr value = get_atom_ptr(self).getValue(
            deref((<Atom>key).handle))
        if (value != NULL):
            return createProtoAtom(value)
        else:
            return None

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

    property xincoming:
        def __get__(self):
            cdef vector[cHandle] handle_vector
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if atom_ptr == NULL:   # avoid null-pointer deref
                return None
            atom_ptr.getIncomingSet(back_inserter(handle_vector))

            # This code is the same for all the x iterators but there is no
            # way in Cython to yield out of a cdef function and no way to pass a
            # vector into a Python def function, so we have to repeat code. ARGGG!
            cdef vector[cHandle].iterator c_handle_iter
            cdef cHandle current_c_handle
            c_handle_iter = handle_vector.begin()
            while c_handle_iter != handle_vector.end():
                current_c_handle = deref(c_handle_iter)
                yield Atom(void_from_candle(current_c_handle),self)
                inc(c_handle_iter)

    def incoming_by_type(self, Type type):
        cdef vector[cHandle] handle_vector
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            return None
        atom_ptr.getIncomingSetByType(back_inserter(handle_vector), type)
        return convert_handle_seq_to_python_list(handle_vector, self.atomspace)

    def xincoming_by_type(self, Type type):
        cdef vector[cHandle] handle_vector
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            return None
        atom_ptr.getIncomingSetByType(back_inserter(handle_vector), type)

        # This code is the same for all the x iterators but there is no
        # way in Cython to yield out of a cdef function and no way to pass a
        # vector into a Python def function, so we have to repeat code. ARGGG!
        cdef vector[cHandle].iterator c_handle_iter
        cdef cHandle current_c_handle
        c_handle_iter = handle_vector.begin()
        while c_handle_iter != handle_vector.end():
            current_c_handle = deref(c_handle_iter)
            yield Atom(void_from_candle(current_c_handle), self.atomspace)
            inc(c_handle_iter)

    property type:
        def __get__(self):
            cdef cAtom* atom_ptr
            if self._atom_type is None:
                atom_ptr = self.handle.atom_ptr()
                if atom_ptr == NULL:   # avoid null-pointer deref
                    return None
                self._atom_type = atom_ptr.get_type()
            return self._atom_type

    property type_name:
        def __get__(self):
            return get_type_name(self.type)

    property t:
        def __get__(self):
            return self.type

    def truth_value(self, mean, count):
        self.tv = TruthValue(mean, count)
        return self

    def handle_ptr(self):
        return PyLong_FromVoidPtr(self.handle)

    def is_node(self):
        return is_a(self.t, types.Node)

    def is_link(self):
        return is_a(self.t, types.Link)

    def is_a(self,t):
        return is_a(self.t, t)

    def long_string(self):
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr != NULL:
            return atom_ptr.to_string().decode('UTF-8')
        return ""

    def __str__(self):
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr != NULL:
            cs = atom_ptr.to_short_string()
            return cs.decode('UTF-8')
        return ""

    def __repr__(self):
        return self.long_string()

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
