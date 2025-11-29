from libcpp cimport bool
from libcpp.set cimport set as cpp_set
from libcpp.vector cimport vector
from libcpp.memory cimport static_pointer_cast
from libcpp.string cimport string as cpp_string
from cython.operator cimport dereference as deref, preincrement as inc
from opencog.type_ctors cimport cPythonException
import cython

# from atomspace cimport *

cdef extern from "Python.h":
    bint PyBool_Check(object)

# tvkey holds a pointer to (PredicateNode "*-TruthValueKey-*").
# This is technically obsolete, but is heavily used in the unit tests.
# So, for now, leave it as a weirdo backwards compatibility hack,
# XXX FIXME -- remove this someday.
tvkey = create_python_value_from_c_value(<cValuePtr>(truth_key()))

# @todo use the guide here to separate out into a hierarchy
# http://wiki.cython.org/PackageHierarchy

@cython.boundscheck(False)
@cython.wraparound(False)
cdef convert_handle_seq_to_python_list(vector[cHandle] handles):
    cdef vector[cHandle].iterator handle_iter
    cdef cHandle handle
    cdef Py_ssize_t size = <Py_ssize_t>handles.size()
    cdef list result = [None] * size
    cdef Py_ssize_t idx = 0
    handle_iter = handles.begin()
    while handle_iter != handles.end():
        handle = deref(handle_iter)
        result[idx] = create_python_value_from_c_value(<cValuePtr&>(handle, handle.get()))
        inc(handle_iter)
        idx += 1
    return result

cdef convert_handle_set_to_python_list(cpp_set[cHandle] handles):
    return [create_python_value_from_c_value(<cValuePtr&>(h, h.get())) for h in handles]


@cython.boundscheck(False)
@cython.wraparound(False)
cdef vector[cHandle] atom_list_to_vector(list lst):
    cdef vector[cHandle] handle_vector
    handle_vector.reserve(len(lst))
    for atom in lst:
        if isinstance(atom, Atom):
            handle_vector.push_back(<cHandle&>(<Atom>atom).shared_ptr)
        else:
            raise TypeError("Outgoing set should contain atoms, got {0} instead".format(type(atom)))
    return handle_vector


cdef extern from "opencog/cython/opencog/ExecuteStub.h" namespace "opencog":
    cdef cValuePtr c_do_execute_atom "do_execute"(cAtomSpace*, cHandle) nogil except +


cdef AtomSpace_factoid(cHandle to_wrap):
    if to_wrap.get() == NULL:
        raise RuntimeError("Cannot create AtomSpace from null pointer")
    cdef AtomSpace instance = AtomSpace.__new__(AtomSpace)
    instance.shared_ptr = static_pointer_cast[cValue, cAtom](to_wrap)
    instance.parent_atomspace = None
    return instance


cdef object raise_python_exception_from_cpp(const cPythonException& exc):
    """Convert C++ PythonException back to native Python exception"""
    cdef cpp_string exc_type_cpp = exc.get_python_exception_type()
    cdef const char* exc_msg_c = exc.get_message()

    exc_type_name = exc_type_cpp.decode('utf-8')
    exc_message = exc_msg_c.decode('utf-8') if exc_msg_c else ""

    # Map to Python exception type
    exception_map = {
        'TypeError': TypeError,
        'ValueError': ValueError,
        'ZeroDivisionError': ZeroDivisionError,
        'AttributeError': AttributeError,
        'ImportError': ImportError,
        'ModuleNotFoundError': ModuleNotFoundError,
        'KeyError': KeyError,
        'IndexError': IndexError,
        'NameError': NameError,
        'RuntimeError': RuntimeError,
    }

    exc_class = exception_map.get(exc_type_name, RuntimeError)
    raise exc_class(exc_message)


cdef class AtomSpace(Atom):
    # these are defined in atomspace.pxd:
    # cdef object parent_atomspace

    def __init__(self, object parent=None):
        """Create a new AtomSpace.

        Args:
            parent: Optional parent atomspace reference (for Python tracking only)

        Creates a new C++ AtomSpace. Does NOT create a child atomspace -
        use create_child_atomspace() for that.

        To wrap an existing C++ AtomSpace pointer, use AtomSpace_factoid().
        """
        cdef cHandle new_as = createAtomSpace(<cAtomSpace*> NULL)
        self.shared_ptr = static_pointer_cast[cValue, cAtom](new_as)
        self.parent_atomspace = parent

    def __richcmp__(as_1, as_2, int op):
        if not isinstance(as_1, AtomSpace) or not isinstance(as_2, AtomSpace):
            return NotImplemented
        cdef AtomSpace atomspace_1 = <AtomSpace>as_1
        cdef AtomSpace atomspace_2 = <AtomSpace>as_2

        is_equal = True
        if atomspace_1.shared_ptr != atomspace_2.shared_ptr:
            is_equal = False
        if op == 2: # ==
            return is_equal
        elif op == 3: # !=
            return not is_equal

    def add_atom(self, Atom atom):
        cdef cHandle h = <cHandle&>atom.shared_ptr
        cdef cHandle result
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            result = asp.add_atom(h)
        if result == result.UNDEFINED:
            return None
        return create_python_value_from_c_value(<cValuePtr&>(result, result.get()))

    def add_node(self, Type t, atom_name):
        """ Add Node to AtomSpace
        @todo support type name for type.
        @returns the newly created Atom
        """
        # See comments on encoding "invalid" bytes in type_ctors.pyx
        # These bytes are from Microsoft Windows doggie litter.
        cdef string name = atom_name.encode('UTF-8', 'surrogateescape')
        cdef cHandle result
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            result = asp.xadd_node(t, name)

        if result == result.UNDEFINED: return None
        return Atom.createAtom(result);

    def add_link(self, Type t, outgoing):
        """ Add Link to AtomSpace
        @todo support type name for type.
        @returns handle referencing the newly created Atom
        """
        # create temporary cpp vector
        cdef vector[cHandle] handle_vector = atom_list_to_vector(outgoing)
        cdef cHandle result
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            result = asp.xadd_link(t, handle_vector)
        if result == result.UNDEFINED: return None
        return Atom.createAtom(result);

    def is_valid(self, atom):
        """ Check whether the passed handle refers to an actual atom
        """
        try:
            assert isinstance(atom, Atom)
        except AssertionError:
            raise TypeError("Need Atom object")
        cdef bint result
        cdef cHandle h = <cHandle&>(<Atom>atom).shared_ptr
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            result = asp.is_valid_handle(h)
        return result

    def remove(self, Atom atom, recursive=False):
        """ Removes an atom from the atomspace
        atom --  The Atom of the atom to be removed.
        recursive -- Recursive-removal flag; if set, then all links
            that contain this atom will be removed. If not set, the
            incoming set of this atom must be empty, as otherwise
            the atom cannot be removed.

        Returns True if the Atom was successfully removed. False, otherwise.

        """
        cdef bint recurse = recursive
        cdef bint result
        cdef cHandle h = <cHandle&>atom.shared_ptr
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            result = asp.extract_atom(h, recurse)
        return result

    def clear(self):
        """ Remove all atoms from the AtomSpace """
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            asp.clear()

    def set_value(self, Atom atom, Atom key, Value value):
        """ Set the value on the atom at key
        Returns the atom (which may be a new atom instance)
        """
        cdef cHandle result
        cdef cHandle atom_h = <cHandle&>atom.shared_ptr
        cdef cHandle key_h = <cHandle&>key.shared_ptr
        cdef cValuePtr val_ptr = value.get_c_value_ptr()
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            result = asp.set_value(atom_h, key_h, val_ptr)
        return Atom.createAtom(result)

    # Methods to make the atomspace act more like a standard Python container
    def __contains__(self, atom):
        """ Custom checker to see if object is in AtomSpace """
        cdef cHandle result
        cdef cHandle h = <cHandle&>(<Atom>atom).shared_ptr
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            result = asp.get_atom(h)
        return result != result.UNDEFINED

    def __len__(self):
        """ Return the number of atoms in the AtomSpace """
        return self.size()

    def __iter__(self):
        """ Support iterating across all atoms in the atomspace """
        return iter(self.get_atoms_by_type(types.Atom))

    def size(self):
        """ Return the number of atoms in the AtomSpace """
        cdef int result
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            result = asp.get_size()
        return result

    # query methods
    def get_atoms_by_type(self, Type t, subtype = True):
        cdef vector[cHandle] handle_vector
        cdef bint subt = subtype
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            asp.get_handles_by_type(handle_vector,t,subt)
        return convert_handle_seq_to_python_list(handle_vector)

    def is_node_in_atomspace(self, Type t, s):
        cdef string name = s.encode('UTF-8', 'surrogateescape')
        cdef cHandle result
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            result = asp.xget_handle(t, name)
        return result != result.UNDEFINED

    def is_link_in_atomspace(self, Type t, outgoing):
        cdef vector[cHandle] handle_vector = atom_list_to_vector(outgoing)
        cdef cHandle result
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        with nogil:
            result = asp.xget_handle(t, handle_vector)
        return result != result.UNDEFINED

    def execute(self, Atom atom):
        if atom is None:
            raise ValueError("No atom provided!")
        cdef cValuePtr c_value_ptr
        cdef cHandle h = <cHandle&>atom.shared_ptr
        cdef cAtomSpace* asp = <cAtomSpace*>self.shared_ptr.get()
        try:
            with nogil:
                c_value_ptr = c_do_execute_atom(asp, h)
            return create_python_value_from_c_value(c_value_ptr)
        except RuntimeError as e:
            cpp_except_to_pyerr(e)

cdef api object py_atom(cHandle h):
    return create_python_value_from_c_value(<cValuePtr&>(h, h.get()))

# Older cythons (before 2024) get compiler errors with the noexcept
# keyword. Newer cythons without it get nag notes about optimization.
# ubuntu-24.04 works; ubuntu-22.04 fails; debian-bokworm (2023) fails
# cdef api cValuePtr py_value_ptr(object py_value) noexcept with gil:
cdef api cValuePtr py_value_ptr(object py_value) with gil:
    """Extract C++ ValuePtr from Python Value object.

    Handles Python booleans by converting them to BoolValue.
    Returns a copy of the C++ shared_ptr (24-byte smart pointer).
    The copy increments the refcount, so the C++ object stays alive.
    """
    cdef bool bval

    # Handle Python boolean (True or False) -> BoolValue
    if PyBool_Check(py_value):
        bval = (py_value == True)
        return <cValuePtr>c_createBoolValue_single(bval)

    # Handle Value objects (Atoms, BoolValue, etc.)
    if not isinstance(py_value, Value):
        return cValuePtr()  # Return null/empty shared_ptr
    return (<Value>py_value).shared_ptr

def create_child_atomspace(object atomspace):
    cdef cAtomSpace* parent_asp = <cAtomSpace*>(<AtomSpace>atomspace).shared_ptr.get()
    cdef cHandle asp = createAtomSpace(parent_asp)
    cdef AtomSpace result = AtomSpace_factoid(asp)
    result.parent_atomspace = atomspace
    return result

# ====================== end of file ============================
