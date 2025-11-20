from cpython cimport PyLong_FromLongLong
from cpython.object cimport Py_LT, Py_EQ, Py_GT, Py_LE, Py_NE, Py_GE
from libcpp.set cimport set as cpp_set

# from atomspace cimport Atom


cdef void cpp_except_to_pyerr(object e):
    """
    Parse RuntimeError message to extract and re-raise original Python exception.

    Examines RuntimeError messages to detect if they contain Python exceptions
    that were converted to C++ and back. Re-raises with the proper exception type.
    """
    error_msg = str(e)

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
    }

    for exc_type_name, exc_class in exception_map.items():
        if exc_type_name + ':' in error_msg:
            raise exc_class(error_msg)

    # If we can't identify the type, re-raise as-is
    raise e


# Atom wrapper object
cdef class Atom(Value):

    def __cinit__(self):
        self._name = None
        self._outgoing = None
        self._atomspace = None
        self._hash = None

    @staticmethod
    cdef Atom createAtom(const cHandle& handle):
        """Factory method to construct Atom from C++ Handle.

        Uses __new__ to bypass constructors, then directly assigns the shared_ptr.
        """
        cdef cHandle nch = handle
        cdef cAtom* atom_ptr = nch.get()
        if atom_ptr == NULL:
            raise RuntimeError("Atom cannot be a null pointer")

        cdef Atom instance = Atom.__new__(Atom)
        instance.shared_ptr = <cValuePtr&>(nch, atom_ptr)
        instance._name = None
        instance._outgoing = None
        instance._atomspace = None
        instance._hash = None
        return instance

    @property
    def atomspace(self):
        cdef cAtomSpace* a
        if self._atomspace is None:
            a = (<cHandle&>self.shared_ptr).get().getAtomSpace()
            self._atomspace = AtomSpace_factoid(as_cast(a))
        return self._atomspace

    @property
    def name(self):
        cdef cAtom* atom_ptr
        if self._name is None:
            atom_ptr = <cAtom*>self.shared_ptr.get()
            if atom_ptr.is_node():
                self._name = atom_ptr.get_name().decode('UTF-8')
            else:
                self._name = ""
        return self._name

    def get_value(self, key):
        cdef cHandle key_handle = <cHandle&>(<Atom>key).shared_ptr
        cdef cHandle self_handle = <cHandle&>self.shared_ptr
        cdef cValuePtr value
        with nogil:
            value = self_handle.get().getValue(key_handle)
        if value.get() == NULL:
            return None
        return create_python_value_from_c_value(value)

    def get_keys(self):
        """
        Returns the keys of Values associated with this Atom.

        :returns: A list of Atoms.
        """
        cdef cHandle self_handle = <cHandle&>self.shared_ptr
        cdef cpp_set[cHandle] keys
        with nogil:
            keys = self_handle.get().getKeys()
        return convert_handle_set_to_python_list(keys)

    def to_list(self):
        cdef cAtom* atom_ptr
        cdef vector[cHandle] hvec
        if self._outgoing is None:
            atom_ptr = <cAtom*>self.shared_ptr.get()
            if atom_ptr.is_link():
                hvec = atom_ptr.getOutgoingSet()
                self._outgoing = convert_handle_seq_to_python_list(hvec)
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
        cdef cAtom* atom_ptr = <cAtom*>self.shared_ptr.get()
        with nogil:
            handle_vector = atom_ptr.getIncomingSet()
        return convert_handle_seq_to_python_list(handle_vector)

    def incoming_by_type(self, Type type):
        cdef vector[cHandle] handle_vector
        cdef cAtom* atom_ptr = <cAtom*>self.shared_ptr.get()
        with nogil:
            handle_vector = atom_ptr.getIncomingSetByType(type)
        return convert_handle_seq_to_python_list(handle_vector)

    def is_executable(self):
        cdef cAtom* atom_ptr = <cAtom*>self.shared_ptr.get()
        return atom_ptr.is_executable()

    def execute(self):
        """
        Execute the Atom, returning the result of execution.

        :returns: A Value
        """
        cdef cAtom* atom_ptr = <cAtom*>self.shared_ptr.get()
        if not atom_ptr.is_executable():
            return self

        cdef cValuePtr c_value_ptr
        try:
            with nogil:
                c_value_ptr = atom_ptr.execute()
            return create_python_value_from_c_value(c_value_ptr)
        except RuntimeError as e:
            cpp_except_to_pyerr(e)

    def __richcmp__(self, other, int op):
        if op == Py_NE:
            if not isinstance(other, Atom):
                return True
            return not self.__eq(other)
        if not isinstance(other, Atom):
            return False
        if op == Py_LT:
            return self.__lt(other)
        if op == Py_EQ:
            return self.__eq(other)
        if op == Py_GT:
            return other.__lt(self)
        if op == Py_LE:
            return not other.__lt(self)
        if op == Py_GE:
            return not self.__lt(other)
        raise RuntimeError("unexpected comparison kind: {0}".format(op))

    def __lt(self, other):
        assert isinstance(other, Atom), "Only Atom instances are comparable with atoms"
        cdef cAtom* p = <cAtom*>self.shared_ptr.get()
        cdef cAtom* o = <cAtom*>(<Atom>other).shared_ptr.get()
        return deref(p) < deref(o)

    def __eq(self, other):
        if not isinstance(other, Atom):
            return False
        cdef cAtom* p = <cAtom*>self.shared_ptr.get()
        cdef cAtom* o = <cAtom*>(<Atom>other).shared_ptr.get()
        return deref(p) == deref(o)

    def __hash__(self):
        if self._hash is None:
            self._hash = PyLong_FromLongLong((<cAtom*>self.shared_ptr.get()).get_hash())
        return self._hash
