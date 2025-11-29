# Cython/distutils can only handle a single file as the source for
# a python module.  Since it is helpful to be able to split the binding
# code into separate files, we just include them here.
#
# Note that the ordering of include statements may influence whether
# things work or not

include "value.pyx"
include "bool_value.pyx"
include "float_value.pyx"
include "link_value.pyx"
include "queue_value.pyx"
include "string_value.pyx"
include "uniset_value.pyx"
include "void_value.pyx"
include "atom.pyx"
include "nameserver.pyx"
include "atomspace_details.pyx"

# -----------------------------------------------------------------
# Module initialization: Create and set a default atomspace if one
# doesn't already exist. This allows type constructors to work
# without requiring explicit set_default_atomspace() calls.

from opencog.type_ctors cimport get_context_atomspace, push_context_atomspace

cdef void _init_default_atomspace():
    cdef cHandle default_as
    cdef cHandle new_as

    default_as = handle_cast(get_context_atomspace())
    if default_as.get() == NULL:
        new_as = createAtomSpace(<cAtomSpace*> NULL)
        push_context_atomspace(new_as)

_init_default_atomspace()
