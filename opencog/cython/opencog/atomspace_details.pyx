from libcpp cimport bool
from libcpp.vector cimport vector
from cython.operator cimport dereference as deref, preincrement as inc

from atomspace cimport *

# @todo use the guide here to separate out into a hierarchy
# http://wiki.cython.org/PackageHierarchy

cdef api string get_path_as_string() with gil:
    import sys
    cdef bytes c_str = str(sys.path)
    return string(c_str)

cdef convert_handle_seq_to_python_list(vector[cHandle] handles, AtomSpace atomspace):
    cdef vector[cHandle].iterator handle_iter
    cdef cHandle handle
    result = []
    handle_iter = handles.begin()
    while handle_iter != handles.end():
        handle = deref(handle_iter)
        result.append(Atom(void_from_candle(handle), atomspace))
        inc(handle_iter)
    return result

cdef AtomSpace_factory(cAtomSpace *to_wrap):
    cdef AtomSpace instance = AtomSpace.__new__(AtomSpace)
    instance.atomspace = to_wrap
    instance.owns_atomspace = False
    return instance

cdef class AtomSpace:
    # these are defined in atomspace.pxd:
    #cdef cAtomSpace *atomspace
    #cdef bint owns_atomspace

    def __cinit__(self):
        self.owns_atomspace = False

    # A tacky hack to pass in a pointer to an atomspace from C++-land.
    # basically, pass an int, and cast it to the C++ pointer.  This
    # works, but is not very safe, and has a certain feeling of "ick"
    # about it.  But I can't find any better way.
    def __init__(self, long addr = 0):
        if (addr == 0) :
            self.atomspace = new cAtomSpace()
            self.owns_atomspace = True
            attentionbank(self.atomspace)
        else :
            self.atomspace = <cAtomSpace*> PyLong_AsVoidPtr(addr)
            self.owns_atomspace = False
            attentionbank(self.atomspace)

    def __dealloc__(self):
        if self.owns_atomspace:
            if self.atomspace:
                del self.atomspace
                attentionbank(<cAtomSpace*> PyLong_AsVoidPtr(0))

    def __richcmp__(as_1, as_2, int op):
        if not isinstance(as_1, AtomSpace) or not isinstance(as_2, AtomSpace):
            return NotImplemented
        cdef AtomSpace atomspace_1 = <AtomSpace>as_1
        cdef AtomSpace atomspace_2 = <AtomSpace>as_1

        cdef cAtomSpace* c_atomspace_1 = atomspace_1.atomspace
        cdef cAtomSpace* c_atomspace_2 = atomspace_2.atomspace

        is_equal = True
        if c_atomspace_1 != c_atomspace_2:
            is_equal = False
        if op == 2: # ==
            return is_equal
        elif op == 3: # !=
            return not is_equal

    def add(self, Type t, name=None, out=None, TruthValue tv=None):
        """ add method that determines exact method to call from type """
        if is_a(t, types.Node):
            assert out is None, "Nodes can't have outgoing sets"
            atom = self.add_node(t, name, tv)
        else:
            assert name is None, "Links can't have names"
            atom = self.add_link(t, out, tv)
        return atom

    def add_node(self, Type t, atom_name, TruthValue tv=None):
        """ Add Node to AtomSpace
        @todo support [0.5,0.5] format for TruthValue.
        @todo support type name for type.
        @returns the newly created Atom
        """
        if self.atomspace == NULL:
            return None
        cdef string name = atom_name.encode('UTF-8')
        cdef cHandle result = self.atomspace.add_node(t, name)

        if result == result.UNDEFINED: return None
        atom = Atom(void_from_candle(result), self);
        if tv :
            atom.tv = tv
        return atom

    def add_link(self, Type t, outgoing, TruthValue tv=None):
        """ Add Link to AtomSpace
        @todo support [0.5,0.5] format for TruthValue.
        @todo support type name for type.
        @returns handle referencing the newly created Atom
        """
        if self.atomspace == NULL:
            return None
        # create temporary cpp vector
        cdef vector[cHandle] handle_vector
        for atom in outgoing:
            if isinstance(atom, Atom):
                handle_vector.push_back(deref((<Atom>(atom)).handle))
        cdef cHandle result
        result = self.atomspace.add_link(t, handle_vector)
        if result == result.UNDEFINED: return None
        atom = Atom(void_from_candle(result), self);
        if tv :
            atom.tv = tv
        return atom

    def is_valid(self, atom):
        """ Check whether the passed handle refers to an actual atom
        """
        if self.atomspace == NULL:
            return False
        try:
            assert isinstance(atom, Atom)
        except AssertionError:
            raise TypeError("Need Atom object")
        if self.atomspace.is_valid_handle(deref((<Atom>atom).handle)):
            return True
        return False

    def remove(self, Atom atom, recursive=True):
        """ Removes an atom from the atomspace
        atom --  The Atom of the atom to be removed.
        recursive -- Recursive-removal flag; if set, then all links
            that contain this atom will be removed. If not set, the
            incoming set of this atom must be empty, as otherwise
            the atom cannot be removed.

        Returns True if the Atom was successfully removed. False, otherwise.

        """
        if self.atomspace == NULL:
            return None
        cdef bint recurse = recursive
        return self.atomspace.remove_atom(deref(atom.handle),recurse)

    def clear(self):
        """ Remove all atoms from the AtomSpace """
        if self.atomspace == NULL:
            return None
        self.atomspace.clear()

    # Methods to make the atomspace act more like a standard Python container
    def __contains__(self, atom):
        """ Custom checker to see if object is in AtomSpace """
        if isinstance(atom, Atom):
            return self.is_valid(atom)
        else:
            return False

    # Maybe this should be called __repr__ ???
    def __str__(self):
        """ Description of the atomspace """
        return ("<Atomspace\n" +
                "   addr: " + hex(<long>self.atomspace) + "\n"
                "   owns: " + str(self.owns_atomspace) + ">\n"
               )

    def __len__(self):
        """ Return the number of atoms in the AtomSpace """
        return self.size()

    def __iter__(self):
        """ Support iterating across all atoms in the atomspace """
        if self.atomspace == NULL:
            return None
        return iter(self.get_atoms_by_type(0))

    def size(self):
        """ Return the number of atoms in the AtomSpace """
        if self.atomspace == NULL:
            return 0
        return self.atomspace.get_size()

    # query methods
    def get_atoms_by_type(self, Type t, subtype = True):
        if self.atomspace == NULL:
            return None
        cdef vector[cHandle] handle_vector
        cdef bint subt = subtype
        self.atomspace.get_handles_by_type(back_inserter(handle_vector),t,subt)
        return convert_handle_seq_to_python_list(handle_vector,self)

    def xget_atoms_by_type(self, Type t, subtype = True):
        if self.atomspace == NULL:
            return None
        cdef vector[cHandle] handle_vector
        cdef bint subt = subtype
        self.atomspace.get_handles_by_type(back_inserter(handle_vector),t,subt)

        # This code is the same for all the x iterators but there is no
        # way in Cython to yield out of a cdef function and no way to pass a
        # vector into a Python def function, so we have to repeat code. ARGGG!
        cdef vector[cHandle].iterator c_handle_iter
        cdef cHandle current_c_handle
        c_handle_iter = handle_vector.begin()
        while c_handle_iter != handle_vector.end():
            current_c_handle = deref(c_handle_iter)
            yield Atom(void_from_candle(current_c_handle), self)
            inc(c_handle_iter)

    def get_atoms_by_av(self, lower_bound, upper_bound=None):
        if self.atomspace == NULL:
            return None
        cdef vector[cHandle] handle_vector
        if upper_bound is not None:
            attentionbank(self.atomspace).get_handles_by_AV(back_inserter(handle_vector),
                    lower_bound, upper_bound)
        else:
            attentionbank(self.atomspace).get_handles_by_AV(back_inserter(handle_vector),
                    lower_bound)
        return convert_handle_seq_to_python_list(handle_vector, self)

    def xget_atoms_by_av(self, lower_bound, upper_bound=None):
        if self.atomspace == NULL:
            return None
        cdef vector[cHandle] handle_vector
        if upper_bound is not None:
            attentionbank(self.atomspace).get_handles_by_AV(back_inserter(handle_vector),
                    lower_bound, upper_bound)
        else:
            attentionbank(self.atomspace).get_handles_by_AV(back_inserter(handle_vector),
                    lower_bound)

        # This code is the same for all the x iterators but there is no
        # way in Cython to yield out of a cdef function and no way to pass a
        # vector into a Python def function, so we have to repeat code. ARGGG!
        cdef vector[cHandle].iterator c_handle_iter
        cdef cHandle current_c_handle
        c_handle_iter = handle_vector.begin()
        while c_handle_iter != handle_vector.end():
            current_c_handle = deref(c_handle_iter)
            yield Atom(void_from_candle(current_c_handle), self)
            inc(c_handle_iter)

    def get_atoms_in_attentional_focus(self):
        if self.atomspace == NULL:
            return None
        cdef vector[cHandle] handle_vector
        attentionbank(self.atomspace).get_handle_set_in_attentional_focus(back_inserter(handle_vector))
        return convert_handle_seq_to_python_list(handle_vector, self)

    def xget_atoms_in_attentional_focus(self):
        if self.atomspace == NULL:
            return None
        cdef vector[cHandle] handle_vector
        attentionbank(self.atomspace).get_handle_set_in_attentional_focus(back_inserter(handle_vector))

        # This code is the same for all the x iterators but there is no
        # way in Cython to yield out of a cdef function and no way to pass a
        # vector into a Python def function, so we have to repeat code. ARGGG!
        cdef vector[cHandle].iterator c_handle_iter
        cdef cHandle current_c_handle
        c_handle_iter = handle_vector.begin()
        while c_handle_iter != handle_vector.end():
            current_c_handle = deref(c_handle_iter)
            yield Atom(void_from_candle(current_c_handle), self)
            inc(c_handle_iter)

    def get_predicates(self,
                       Atom target,
                       Type predicate_type = types.PredicateNode,
                       subclasses=True):
        if self.atomspace == NULL:
            return None
        cdef vector[cHandle] handle_vector
        cdef bint want_subclasses = subclasses
        handle_vector = c_get_predicates(deref(target.handle), predicate_type,
                                         want_subclasses)
        return convert_handle_seq_to_python_list(handle_vector, self)

    def xget_predicates(self,
                        Atom target,
                        Type predicate_type = types.PredicateNode,
                        subclasses=True):
        if self.atomspace == NULL:
            return None
        cdef vector[cHandle] handle_vector
        cdef bint want_subclasses = subclasses
        handle_vector = c_get_predicates(deref(target.handle), predicate_type,
                                         want_subclasses)

        # This code is the same for all the x iterators but there is no
        # way in Cython to yield out of a cdef function and no way to pass a
        # vector into a Python def function, so we have to repeat code. ARGGG!
        cdef vector[cHandle].iterator c_handle_iter
        cdef cHandle current_c_handle
        c_handle_iter = handle_vector.begin()
        while c_handle_iter != handle_vector.end():
            current_c_handle = deref(c_handle_iter)
            yield Atom(void_from_candle(current_c_handle), self)
            inc(c_handle_iter)

    def get_predicates_for(self, Atom target, Atom predicate):
        if self.atomspace == NULL:
            return None
        cdef vector[cHandle] handle_vector
        handle_vector = c_get_predicates_for(deref(target.handle),
                                             deref(predicate.handle))
        return convert_handle_seq_to_python_list(handle_vector, self)

    def xget_predicates_for(self, Atom target, Atom predicate):
        if self.atomspace == NULL:
            return None
        cdef vector[cHandle] handle_vector
        handle_vector = c_get_predicates_for(deref(target.handle),
                                             deref(predicate.handle))

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

    @classmethod
    def include_incoming(cls, atoms):
        """
        Returns the conjunction of a set of atoms and their incoming sets.

        Example:
        self.atomspace.include_incoming(self.atomspace.get_atoms_by_type(types.ConceptNode))
        """
        return list(set(atoms +
                [item for sublist in [atom.incoming for atom in atoms if len(atom.incoming) > 0] for item in sublist]))

    @classmethod
    def include_outgoing(cls, atoms):
        """
        Returns the conjunction of a set of atoms and their outgoing sets.
        Useful when used in combination with include_incoming.

        Example:
        self.atomspace.include_outgoing(
            self.atomspace.include_incoming(self.atomspace.get_atoms_by_type(types.ConceptNode)))
        """
        return list(set(atoms +
                [item for sublist in [atom.out for atom in atoms if len(atom.out) > 0] for item in sublist]))

    def print_list(self):
    #    self.atomspace.print_list()
         pass

cdef api object py_atomspace(cAtomSpace *c_atomspace) with gil:
    cdef AtomSpace atomspace = AtomSpace_factory(c_atomspace)
    return atomspace

cdef api object py_atom(PANDLE lptr, object atomspace):
    cdef Atom atom = Atom(lptr, atomspace)
    return atom
