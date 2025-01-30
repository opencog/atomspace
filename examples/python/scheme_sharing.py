#! /usr/bin/env python3
#
# scheme_sharing.py
#
"""
Demonstration of mixing python and scheme Atomese.
"""

from opencog.atomspace import *
from opencog.type_constructors import *
from opencog.scheme_wrapper import *

# Create an AtomSpace, and let tthe python type constructors
# know about it.
asp = AtomSpace()
set_default_atomspace(asp)

# Add an Atom to the AtomSpace, using python
ConceptNode("this is a test")

# Print the AtomSpace contents, using scheme:
scheme_eval(asp, '(cog-prt-atomspace)')
scheme_eval(asp,
     '(format #t "Yes this is really scheme: ~A\n" (+ 2 2))')

# Add an Atom, using scheme. The scheme_eval_h() function returns
# a handle to that Atom, and python can use that handle, just as if
# that Atom was created natively, in python. Both are the same Atom.
foo_atom = scheme_eval_h(asp, '(Concept "foo")')
print("In python it looks like this:", foo_atom)

# Dump the AtomSpace contents into a list. You'll find both Atoms
# are in there.
print("The AtomSpace contains:" , list(asp))

# THE END. That's All, Folks!
