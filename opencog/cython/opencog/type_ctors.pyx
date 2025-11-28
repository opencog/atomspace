from cython.operator cimport dereference as deref
from libcpp.string cimport string
from libcpp.set cimport set as cpp_set
from opencog.atomspace cimport AtomSpace, Atom, Value
from opencog.atomspace cimport cHandle, cValuePtr, create_python_value_from_c_value
from opencog.atomspace cimport AtomSpace_factoid, handle_cast

from contextlib import contextmanager
from opencog.atomspace import create_child_atomspace

def add_link(Type t, outgoing):

    # Unwrap double-wrapped lists. The type constructors create these.
    if 1 == len(outgoing) and isinstance(outgoing[0], list):
        outgoing = outgoing[0]

    # Use a temporary cpp vector
    cdef vector[cHandle] handle_vector
    for atom in outgoing:
        if isinstance(atom, Atom):
            handle_vector.push_back(<cHandle&>(<Atom>atom).shared_ptr)
        else:
            raise TypeError("outgoing set should contain atoms, got {0} instead".format(type(atom)))

    cdef cHandle result
    result = c_add_link(t, handle_vector)
    if result == result.UNDEFINED: return None
    return create_python_value_from_c_value(<cValuePtr&>(result, result.get()))


def add_node(Type t, atom_name):
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
    # they contain bytes that cannot be converted to UTF-8 using
    # default encoding tables. Such strings typically come from
    # Microsoft, which had a habit of spewing screwball characters
    # into random texts. So, rather than catching python's exception,
    # just escape these bytes. The result is a valid UTF-8 string
    # with the screwy byte properly encoded.
    cdef string name = atom_name.encode('UTF-8', 'surrogateescape')
    cdef cHandle result = c_add_node(t, name)

    if result == result.UNDEFINED: return None
    return create_python_value_from_c_value(<cValuePtr&>(result, result.get()))

def set_thread_atomspace(AtomSpace atomspace):
    """
    Set the default atomspace for the current thread.
    Clears the context stack and pushes the atomspace onto it.
    """
    c_clear_context()
    if atomspace is not None:
        push_context_atomspace(handle_cast(atomspace.shared_ptr))


def push_thread_atomspace(AtomSpace new_atomspace = None):
    """
    Set default atomspace for current thread
    """
    if new_atomspace is None:
        parent_atomspace = get_thread_atomspace()
        if parent_atomspace is None:
            raise RuntimeError("Atomspace is not set!")
        new_atomspace = create_child_atomspace(parent_atomspace)
    push_context_atomspace(handle_cast(new_atomspace.shared_ptr))
    return new_atomspace


def get_thread_atomspace():
    """
    Get the default atomspace for the current thread
    """
    cdef cValuePtr context = get_context_atomspace()
    return AtomSpace_factoid(handle_cast(context))


def pop_thread_atomspace():
    return AtomSpace_factoid(handle_cast(pop_context_atomspace()))
