#! /usr/bin/env python
#
# get_outgoings.py
#
"""
A simple example of how to access the outgoing set of a link.
"""

from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types
from opencog.type_constructors import *

a = AtomSpace()

# Tell the type constructors which atomspace to use.
set_default_atomspace(a)

# Add three nodes
A = ConceptNode('Apple')
B = ConceptNode('Berry')
C = ConceptNode('Cherry')

# Add list link
ABC = ListLink(A, B, C)

# Get the outgoing set of ABC
print("The outgoing set of ABC:\n\n", ABC.out)
