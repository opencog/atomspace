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

    def __cinit__(self, PtrHolder ptr_holder, *args, **kwargs):
        self.handle = <cHandle*>&((<PtrHolder>ptr_holder).shared_ptr)
        self._atom_type = None
        self._name = None
        self._outgoing = None
    
    @staticmethod
    cdef Atom createAtom(const cHandle& handle):
        # Create temporary Handle that is not const, so that we can then
        # use it to create the desired ValuePtr. If we don't do this,
        # then cython warns either of failing to use the correct
        # C++ shared_ptr casting methods, or ir errors out with
        # casting away constness.
        cdef cHandle nch = handle
        return Atom(PtrHolder.create(<shared_ptr[cValue]&>(nch, nch.get())))

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

    def id_string(self):
        return self.get_c_handle().get().id_to_string().decode('UTF-8')

    def get_value(self, key):
        cdef cValuePtr value = self.get_c_handle().get().getValue(
            deref((<Atom>key).handle))
        if value.get() == NULL:
            return None
        return create_python_value_from_c_value(value)

    def get_keys(self):
        """
        Returns the keys of Values associated with this Atom.

        :returns: A list of Atoms.
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
        handle_vector = atom_ptr.getIncomingSet()
        return convert_handle_seq_to_python_list(handle_vector)

    def incoming_by_type(self, Type type):
        cdef vector[cHandle] handle_vector
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            raise RuntimeError("Null Atom!")
        handle_vector = atom_ptr.getIncomingSetByType(type)
        return convert_handle_seq_to_python_list(handle_vector)

    def is_executable(self):
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            raise RuntimeError("Null Atom!")
        return atom_ptr.is_executable()

    def execute(self):
        """
        Execute the Atom, returning the result of execution.

        :returns: A Value
        """
        cdef cAtom* atom_ptr = self.handle.atom_ptr()
        if atom_ptr == NULL:   # avoid null-pointer deref
            raise RuntimeError("Null Atom!")
        if not atom_ptr.is_executable():
            return self

        cdef cValuePtr c_value_ptr
        try:
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
