
# Atom wrapper object
cdef class Atom(object):

    def __cinit__(self, UUID uuid, AtomSpace a):
        self.handle = new cHandle(uuid)

    def __dealloc__(self):
        del self.handle

    def value(self):
        return self.handle.value()

    def __init__(self, UUID uuid, AtomSpace a):
        # self.handle = h is set in __cinit__ above

        # cache the results after first retrieval of
        # immutable properties
        self._atom_type = None
        self._name = None
        self._outgoing = None

        # Not really a cache ... an atom could be moved from one
        # atomspace to another (!)
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
                if atom_ptr.isNode():
                    self._name = atom_ptr.getName()
                else:
                    self._name = ""
            return self._name

    property tv:
        def __get__(self):
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            cdef tv_ptr tvp
            tvp = atom_ptr.getTruthValue()
            if (not tvp.get() or tvp.get().isNullTv()):
                pytv = TruthValue()
                pytv.cobj = new tv_ptr(tvp) # make copy of smart pointer
                return pytv
            return TruthValue(tvp.get().getMean(), tvp.get().getConfidence())
        def __set__(self,truth_value):
            try:
                assert isinstance(truth_value, TruthValue)
            except AssertionError:
                raise TypeError("atom.av property needs a TruthValue object")
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            atom_ptr.setTruthValue(deref((<TruthValue>truth_value)._tvptr()))

    property av:
        def __get__(self):
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            sti = atom_ptr.getSTI()
            lti = atom_ptr.getLTI()
            vlti = atom_ptr.getVLTI()
            return { "sti": sti, "lti": lti, "vlti": vlti }
        def __set__(self, av_dict):
            try:
                assert isinstance(av_dict, dict)
            except AssertionError:
                raise TypeError("atom.av property needs a dictionary object")
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            if av_dict:
                if "sti" in av_dict: sti = av_dict["sti"]
                if "lti" in av_dict: lti = av_dict["lti"]
                if "vlti" in av_dict: vlti = av_dict["vlti"]
            if sti: atom_ptr.setSTI(sti)
            if lti: atom_ptr.setLTI(lti)
            if vlti: atom_ptr.setVLTI(vlti)

    property sti:
        def __get__(self):
            return self.handle.atom_ptr().getSTI()
        def __set__(self,val):
            self.handle.atom_ptr().setSTI(val)

    property lti:
        def __get__(self):
            return self.handle.atom_ptr().getLTI()
        def __set__(self,val):
            self.handle.atom_ptr().setLTI(val)

    property vlti:
        def __get__(self):
            return self.handle.atom_ptr().getVLTI()
        def __set__(self,val):
            self.handle.atom_ptr().setVLTI(val)

    def increment_vlti(self):
        self.handle.atom_ptr().incVLTI()
    def decrement_vlti(self):
        self.handle.atom_ptr().decVLTI()

    def get_out(self):
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        cdef vector[cHandle] handle_vector = atom_ptr.getOutgoingSet()
        return convert_handle_seq_to_python_list(handle_vector, self.atomspace)

    property out:
        def __get__(self):            
            if self._outgoing is None:
                atom_ptr = self.handle.atom_ptr()
                if atom_ptr.isLink():
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
            atom_ptr.getIncomingSet(back_inserter(handle_vector))
            return convert_handle_seq_to_python_list(handle_vector, self.atomspace)

    property xincoming:
        def __get__(self):
            cdef vector[cHandle] handle_vector
            cdef cAtom* atom_ptr = self.handle.atom_ptr()
            atom_ptr.getIncomingSet(back_inserter(handle_vector))

            # This code is the same for all the x iterators but there is no
            # way in Cython to yield out of a cdef function and no way to pass a 
            # vector into a Python def function, so we have to repeat code. ARGGG!
            cdef vector[cHandle].iterator c_handle_iter
            cdef cHandle current_c_handle
            c_handle_iter = handle_vector.begin()
            while c_handle_iter != handle_vector.end():
                current_c_handle = deref(c_handle_iter)
                yield Atom(current_c_handle.value(),self)
                inc(c_handle_iter)

    def incoming_by_type(self, Type type, subtype = True):
        cdef vector[cHandle] handle_vector
        cdef bint subt = subtype
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        atom_ptr.getIncomingSetByType(back_inserter(handle_vector), type, subt)
        return convert_handle_seq_to_python_list(handle_vector, self.atomspace)

    def xincoming_by_type(self, Type type, subtype = True):
        cdef vector[cHandle] handle_vector
        cdef bint subt = subtype
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        atom_ptr.getIncomingSetByType(back_inserter(handle_vector), type, subt)

        # This code is the same for all the x iterators but there is no
        # way in Cython to yield out of a cdef function and no way to pass a 
        # vector into a Python def function, so we have to repeat code. ARGGG!
        cdef vector[cHandle].iterator c_handle_iter
        cdef cHandle current_c_handle
        c_handle_iter = handle_vector.begin()
        while c_handle_iter != handle_vector.end():
            current_c_handle = deref(c_handle_iter)
            yield Atom(current_c_handle.value(), self.atomspace)
            inc(c_handle_iter)

    property type:
        def __get__(self):
            cdef cAtom* atom_ptr
            if self._atom_type is None:
                atom_ptr = self.handle.atom_ptr()
                self._atom_type = atom_ptr.getType()
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
    
    def handle_uuid(self):
        return self.value()

    def is_node(self):
        return is_a(self.t,types.Node)

    def is_link(self):
        return is_a(self.t,types.Link)

    def is_a(self,t):
        return is_a(self.t,t)

    def long_string(self):
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr != NULL:
            return atom_ptr.toString()
        return ""

    def __str__(self):
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr != NULL:
            return atom_ptr.toShortString()
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
        return hash(a1.value())
