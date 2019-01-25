# Cython/distutils can only handle a single file as the source for
# a python module.  Since it is helpful to be able to split the binding
# code into separate files, we just include them here.
#
# Note that the ordering of include statements may influence whether
# things work or not

include "nameserver.pyx"
include "truth_value.pyx"
include "atomspace_details.pyx"
include "atom.pyx"
include "value.pyx"
include "float_value.pyx"
include "string_value.pyx"
include "link_value.pyx"
