from cython.operator cimport dereference as deref
from libcpp.string cimport string
from libcpp.set cimport set as cpp_set
from opencog.atomspace cimport AtomSpace, Atom, TruthValue, Value
from opencog.atomspace cimport cValuePtr, create_python_value_from_c_value
from opencog.atomspace cimport AtomSpace_factory

from contextlib import contextmanager
from opencog.atomspace import create_child_atomspace
from opencog.utilities cimport c_load_file
import warnings


# Avoid recursive intialization
is_initialized = False


def initialize_opencog(AtomSpace atomspace=None):
    """
    Set default atomspace(deprecated feature) and
    create python evaluator singleton object.

    Calling this function should not be needed. Use set_default_atomspace to set default atomspace.
    Python evaluator will be created on first evaluation.
    """
    if atomspace is not None:
        warnings.warn("setting default atomspace with initialize_opencog is deprecated,\
                use set_default_atomspace instead", DeprecationWarning)
        set_default_atomspace(atomspace)
    # Avoid recursive intialization
    global is_initialized
    if is_initialized:
        return
    is_initialized = True
    c_initialize_python()


def finalize_opencog():
    global is_initialized
    if is_initialized:
        c_clear_context()
        c_finalize_python()
    is_initialized = False


@contextmanager
def tmp_atomspace():
    """
    Context manager, to create child atomspace from current default
    """
    parent_atomspace = get_default_atomspace()
    if parent_atomspace is None:
        raise RuntimeError("Default atomspace is None")
    atomspace = create_child_atomspace(parent_atomspace)
    set_default_atomspace(atomspace)
    try:
        yield atomspace
    finally:
        set_default_atomspace(parent_atomspace)


def add_link(Type t, outgoing, TruthValue tv=None):
    # create temporary cpp vector
    cdef vector[cHandle] handle_vector
    for atom in outgoing:
        if isinstance(atom, Atom):
            handle_vector.push_back(deref((<Atom>(atom)).handle))
        else:
            raise TypeError("outgoing set should contain atoms, got {0} instead".format(type(atom)))
    cdef cHandle result
    result = c_add_link(t, handle_vector)
    if result == result.UNDEFINED: return None
    atom = create_python_value_from_c_value(<cValuePtr&>result)
    if tv is not None:
        atom.tv = tv
    return atom


def add_node(Type t, atom_name, TruthValue tv=None):
    """
    Add Node to the atomspace from the current context
    """
    cdef string name = atom_name.encode('UTF-8')
    cdef cHandle result = c_add_node(t, name)

    if result == result.UNDEFINED: return None
    atom = create_python_value_from_c_value(<cValuePtr&>result)
    if tv is not None:
        atom.tv = tv
    return atom


def set_default_atomspace(AtomSpace atomspace):
    """
    Emulate set default atomspace with queue for old code
    """
    c_clear_context() 
    if atomspace is not None:
        push_default_atomspace(atomspace)


def push_default_atomspace(AtomSpace new_atomspace):
    """
    Set default atomspace for current threads
    """
    push_context_atomspace(new_atomspace.atomspace)


def get_default_atomspace():
    """
    Get default atomspace
    """
    cdef cAtomSpace * context = get_context_atomspace()
    if context is NULL:
        return None
    return AtomSpace_factory(context)


def pop_default_atomspace():
    return AtomSpace_factory(pop_context_atomspace())


def load_file(path, AtomSpace atomspace):
    cdef string p = path.encode('utf-8')
    c_load_file(p, deref(atomspace.atomspace))


def is_closed(Atom atom):
    """
    Return True iff the atom is closed, that is does not contain free variables.
    """
    return c_is_closed(atom.get_c_handle())


def get_free_variables(Atom atom):
    """
    Return the list of free variables in a given atom.
    """
    cdef cpp_set[cHandle] variables = c_get_free_variables(atom.get_c_handle())
    return [create_python_value_from_c_value(<cValuePtr&> h) for h in variables]
