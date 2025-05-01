from libcpp cimport bool
from libcpp.set cimport set as cpp_set
from libcpp.vector cimport vector
from cython.operator cimport dereference as deref, preincrement as inc

# from atomspace cimport *


# @todo use the guide here to separate out into a hierarchy
# http://wiki.cython.org/PackageHierarchy

cdef api string get_path_as_string() with gil:
    import sys
    return str(sys.path).encode('UTF-8')

cdef convert_handle_seq_to_python_list(vector[cHandle] handles):
    cdef vector[cHandle].iterator handle_iter
    cdef cHandle handle
    result = []
    handle_iter = handles.begin()
    while handle_iter != handles.end():
        handle = deref(handle_iter)
        value = create_python_value_from_c_value(<cValuePtr&>handle)
        result.append(value)
        inc(handle_iter)
    return result

cdef convert_handle_set_to_python_list(cpp_set[cHandle] handles):
    return [create_python_value_from_c_value(<cValuePtr&> h) for h in handles]


cdef vector[cHandle] atom_list_to_vector(list lst):
    cdef vector[cHandle] handle_vector
    for atom in lst:
        if isinstance(atom, Atom):
            handle_vector.push_back(deref((<Atom>(atom)).handle))
        else:
            raise TypeError("outgoing set should contain atoms, got {0} instead".format(type(atom)))
    return handle_vector


cdef extern from "opencog/cython/opencog/ExecuteStub.h" namespace "opencog":
    cdef cValuePtr c_do_execute_atom "do_execute"(cAtomSpace*, cHandle) except +


cdef AtomSpace_factoid(cValuePtr to_wrap):
    cdef AtomSpace instance = AtomSpace.__new__(AtomSpace)
    instance.asp = to_wrap
    instance.atomspace = <cAtomSpace*> to_wrap.get()
    # print("Debug: atomspace factory={0:x}".format(<long unsigned int>to_wrap.get()))
    return instance


cdef class AtomSpace(Value):
    # these are defined in atomspace.pxd:
    # cdef cAtomSpace *atomspace
    # cdef object parent_atomspace

    # A tacky hack to pass in a pointer to an atomspace from C++-land.
    # basically, pass an int, and cast it to the C++ pointer.  This
    # works, but is not very safe, and has a certain feeling of "ick"
    # about it.  But I can't find any better way.
    def __init__(self, long addr = 0, object parent=None):
        if (addr == 0) :
            self.asp = createAtomSpace(<cAtomSpace*> NULL)
        else :
            self.asp = as_cast(<cAtomSpace*> PyLong_AsVoidPtr(addr))
        self.atomspace = <cAtomSpace*> self.asp.get()
        self.parent_atomspace = parent
        self.ptr_holder = PtrHolder.create(<shared_ptr[void]&>self.asp);

    def __richcmp__(as_1, as_2, int op):
        if not isinstance(as_1, AtomSpace) or not isinstance(as_2, AtomSpace):
            return NotImplemented
        cdef AtomSpace atomspace_1 = <AtomSpace>as_1
        cdef AtomSpace atomspace_2 = <AtomSpace>as_1

        cdef cValuePtr c_atomspace_1 = atomspace_1.asp
        cdef cValuePtr c_atomspace_2 = atomspace_2.asp

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

    def add_atom(self, Atom atom):
        cdef cHandle result = self.atomspace.add_atom(atom.get_c_handle())
        if result == result.UNDEFINED:
            return None
        return create_python_value_from_c_value(<cValuePtr&>result)

    def add_node(self, Type t, atom_name, TruthValue tv=None):
        """ Add Node to AtomSpace
        @todo support [0.5,0.5] format for TruthValue.
        @todo support type name for type.
        @returns the newly created Atom
        """
        if self.atomspace == NULL:
            raise RuntimeError("Null AtomSpace!")

        # See comments on encoding "invalid" bytes in utilities.pyx
        # These bytes are from Microsoft Windows doggie litter.
        cdef string name = atom_name.encode('UTF-8', 'surrogateescape')
        cdef cHandle result = self.atomspace.xadd_node(t, name)

        if result == result.UNDEFINED: return None
        atom = Atom.createAtom(result);
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
            raise RuntimeError("Null AtomSpace!")
        # create temporary cpp vector
        cdef vector[cHandle] handle_vector = atom_list_to_vector(outgoing)
        cdef cHandle result
        result = self.atomspace.xadd_link(t, handle_vector)
        if result == result.UNDEFINED: return None
        atom = Atom.createAtom(result);
        if tv :
            atom.tv = tv
        return atom

    def is_valid(self, atom):
        """ Check whether the passed handle refers to an actual atom
        """
        if self.atomspace == NULL:
            raise RuntimeError("Null AtomSpace!")
        try:
            assert isinstance(atom, Atom)
        except AssertionError:
            raise TypeError("Need Atom object")
        if self.atomspace.is_valid_handle(deref((<Atom>atom).handle)):
            return True
        return False

    def remove(self, Atom atom, recursive=False):
        """ Removes an atom from the atomspace
        atom --  The Atom of the atom to be removed.
        recursive -- Recursive-removal flag; if set, then all links
            that contain this atom will be removed. If not set, the
            incoming set of this atom must be empty, as otherwise
            the atom cannot be removed.

        Returns True if the Atom was successfully removed. False, otherwise.

        """
        if self.atomspace == NULL:
            raise RuntimeError("Null AtomSpace!")
        cdef bint recurse = recursive
        return self.atomspace.extract_atom(deref(atom.handle),recurse)

    def clear(self):
        """ Remove all atoms from the AtomSpace """
        if self.atomspace == NULL:
            raise RuntimeError("Null AtomSpace!")
        self.atomspace.clear()

    def set_value(self, Atom atom, Atom key, Value value):
        """ Set the value on the atom at key
        """
        if self.atomspace == NULL:
            raise RuntimeError("Null AtomSpace!")
        self.atomspace.set_value(deref(atom.handle), deref(key.handle),
                                 value.get_c_value_ptr())

    def set_truthvalue(self, Atom atom, TruthValue tv):
        """ Set the truth value on atom
        """
        if self.atomspace == NULL:
            raise RuntimeError("Null AtomSpace!")
        self.atomspace.set_truthvalue(deref(atom.handle), deref(tv._tvptr()))

    # Methods to make the atomspace act more like a standard Python container
    def __contains__(self, atom):
        """ Custom checker to see if object is in AtomSpace """
        cdef cHandle result
        result = self.atomspace.get_atom(deref((<Atom>(atom)).handle))
        return result != result.UNDEFINED

    # Maybe this should be called __repr__ ???
    def __str__(self):
        """ Description of the atomspace """
        return ("<AtomSpace\n" +
                "   addr: " + hex(<long>self.atomspace) + "\n" +
                "   name: " + self.atomspace.get_name().decode('UTF-8') + ">\n"
               )

    def __len__(self):
        """ Return the number of atoms in the AtomSpace """
        return self.size()

    def __iter__(self):
        """ Support iterating across all atoms in the atomspace """
        if self.atomspace == NULL:
            raise RuntimeError("Null AtomSpace!")
        return iter(self.get_atoms_by_type(0))

    def size(self):
        """ Return the number of atoms in the AtomSpace """
        if self.atomspace == NULL:
            raise RuntimeError("Null AtomSpace!")
        return self.atomspace.get_size()

    # query methods
    def get_atoms_by_type(self, Type t, subtype = True):
        if self.atomspace == NULL:
            raise RuntimeError("Null AtomSpace!")
        cdef vector[cHandle] handle_vector
        cdef bint subt = subtype
        self.atomspace.get_handles_by_type(handle_vector,t,subt)
        return convert_handle_seq_to_python_list(handle_vector)

    def is_node_in_atomspace(self, Type t, s):
        cdef string name = s.encode('UTF-8', 'surrogateescape')
        result = self.atomspace.xget_handle(t, name)
        return result != result.UNDEFINED

    def is_link_in_atomspace(self, Type t, outgoing):
        cdef vector[cHandle] handle_vector = atom_list_to_vector(outgoing)
        result = self.atomspace.xget_handle(t, handle_vector)
        return result != result.UNDEFINED

    def execute(self, Atom atom):
        if atom is None:
            raise ValueError("No atom provided!")
        cdef cValuePtr c_value_ptr = c_do_execute_atom(
            self.atomspace, deref(atom.handle))
        return create_python_value_from_c_value(c_value_ptr)

cdef api object py_atomspace(cValuePtr c_atomspace) with gil:
    cdef AtomSpace atomspace = AtomSpace_factoid(c_atomspace)
    return atomspace

cdef api object py_atom(const cHandle& h):
    cdef Atom atom = Atom.createAtom(h)
    return atom

def create_child_atomspace(object atomspace):
    cdef cValuePtr asp = createAtomSpace((<AtomSpace>(atomspace)).atomspace)
    cdef AtomSpace result = AtomSpace_factoid(asp)
    result.parent_atomspace = atomspace
    return result

# ====================== end of file ============================
