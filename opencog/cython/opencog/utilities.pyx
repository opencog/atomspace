from cython.operator cimport dereference as deref
from libcpp.string cimport string
from libcpp.set cimport set as cpp_set
from opencog.atomspace cimport AtomSpace, Atom, TruthValue, Value
from opencog.atomspace cimport cValuePtr, create_python_value_from_c_value
from opencog.atomspace cimport AtomSpace_factoid

from contextlib import contextmanager
from opencog.atomspace import create_child_atomspace
import warnings


# Avoid recursive initialization
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
    # Avoid recursive initialization
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

    # Unwrap double-wrapped lists. The type constructors create these.
    if 1 == len(outgoing) and isinstance(outgoing[0], list):
        outgoing = outgoing[0]

    # Use a temporary cpp vector
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

    # NumberNodes can take lists of numbers.
    if type(atom_name) is list :
        atom_name = ' '.join(map(str, atom_name))

    # NumberNodes can be single numbers.
    if type(atom_name) is int :
        atom_name = str(atom_name)

    if type(atom_name) is float :
        atom_name = str(atom_name)

    # Valid strings include those coming from e.g. iso8859-NN
    # filenames, which break when shoved through UTF-8 because
    # they cotain bytes that cannot be converted to UTF-8 using
    # default encoding tables. Such string typically come from
    # Microsoft, which had a habit of spewing screwball characters
    # into random texts. So, rather than catching python's exception,
    # just escape these bytes. The result is a valid UTF-8 string
    # with the screwy byte properly encoded.
    cdef string name = atom_name.encode('UTF-8', 'surrogateescape')
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
    push_context_atomspace(new_atomspace.asp)


def get_default_atomspace():
    """
    Get default atomspace
    """
    cdef cValuePtr context = get_context_atomspace()
    return AtomSpace_factoid(context)


def pop_default_atomspace():
    return AtomSpace_factoid(pop_context_atomspace())


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
