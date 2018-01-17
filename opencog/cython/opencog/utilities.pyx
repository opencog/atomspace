from opencog.atomspace cimport AtomSpace
from opencog.type_constructors import set_type_ctor_atomspace

# Avoid recursive intialization
is_initialized = False

def initialize_opencog(AtomSpace atomspace):

    # Avoid recursive intialization
    global is_initialized
    if is_initialized:
        set_type_ctor_atomspace(atomspace)
        return
    is_initialized = True

    c_initialize_opencog(atomspace.atomspace)
    set_type_ctor_atomspace(atomspace)

def finalize_opencog():
    global is_initialized
    if is_initialized:
        c_finalize_opencog()

    set_type_ctor_atomspace(None)
