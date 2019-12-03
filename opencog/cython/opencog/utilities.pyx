from contextlib import contextmanager
from opencog.atomspace cimport AtomSpace
from opencog.atomspace import create_child_atomspace
from opencog.type_constructors import get_default_atomspace, set_default_atomspace
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
        set_default_atomspace(None)
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

