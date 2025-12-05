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
    Set the AtomSpace used in the current thread.
    """
    if atomspace is not None:
        set_frame(handle_cast(atomspace.shared_ptr))


def push_thread_atomspace():
    """
    Create a temporary scratch-space AtomSpace for the current thread.
    Everything in the current AtomSpace will be visible in this
    scratch space. Any changes will be made ONLY to the temporary;
    those changes will be discarded when the temporary is popped.

    If you need to make some changes permanent, you can either copy
    those Atoms into whatever desired AtomSpace you wish. Alternately,
    disable the copy-on-write (COW) flag on *this* AtomSpace; this
    will cause changes to pass through to the base AtomSpace.

    Use pop_thread_atomspace() to pop.
    """
    push_frame()
    return get_thread_atomspace()


def get_thread_atomspace():
    """
    Get the AtomSpace being used in this thread.
    """
    cdef cValuePtr context = get_frame()
    return AtomSpace_factoid(handle_cast(context))


def pop_thread_atomspace():
    """
    Pop the temporary scratch-space AtomSpace from the stack.

    Ths assumes the stack was previously pushed with
    push_thread_atomspace().
    """
    pop_frame()
