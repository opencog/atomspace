#! /usr/bin/env python3
#
# scheme_sharing.py
#
"""
Demonstration of mixing python and scheme Atomese.
"""

from opencog.atomspace import *
from opencog.scheme import *

# Add an Atom to the AtomSpace, using python
ConceptNode("this is a test")

# Print the AtomSpace contents, using scheme:
print("Printing the AtomSpace from scheme:",
    scheme_eval('(cog-prt-atomspace)'))
print("Printing result of running some scheme code:",
    scheme_eval(
        '(format #t "Yes this is really scheme: ~A\n" (+ 2 2))'))

# Add an Atom, using scheme. The scheme_eval_v() function returns
# a handle to that Atom, and python can use that handle, just as if
# that Atom was created natively, in python. Both are the same Atom.
foo_atom = scheme_eval_v('(Concept "foo")')
print("In python it looks like this:", foo_atom)

# Dump the AtomSpace contents into a list. You'll find both Atoms
# are in there.
asp = get_thread_atomspace()
print("The AtomSpace contains:" , list(asp))

# THE END. That's All, Folks!
