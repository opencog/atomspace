from contextlib import contextmanager
from opencog.atomspace cimport AtomSpace
from opencog.atomspace import create_child_atomspace
from opencog.type_constructors import get_type_ctor_atomspace, set_type_ctor_atomspace

# Avoid recursive intialization
is_initialized = False

def initialize_opencog(AtomSpace atomspace):

    # Avoid recursive intialization
    global is_initialized
    if is_initialized:
        set_type_ctor_atomspace(atomspace)
        return
    is_initialized = True

    c_initialize_python(atomspace.atomspace)
    set_type_ctor_atomspace(atomspace)

def finalize_opencog():
    global is_initialized
    if is_initialized:
        set_type_ctor_atomspace(None)
        c_finalize_python()

    is_initialized = False


@contextmanager
def tmp_atomspace():
    """
    Context manager, to create child atomspace from current default 
    """
    parent_atomspace = get_type_ctor_atomspace()
    if parent_atomspace is None:
        raise RuntimeError("Default atomspace is None")
    atomspace = create_child_atomspace(parent_atomspace)
    initialize_opencog(atomspace)
    try:
        yield atomspace
    finally:
        initialize_opencog(parent_atomspace)
