#! /usr/bin/env python
#
# create_atoms_simple.py
#
"""
A nicer way of creating atoms in the AtomSpace.
"""

from opencog.atomspace import AtomSpace, TruthValue, Atom
from opencog.atomspace import types
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *


a = AtomSpace()

# Tell the type constructors which atomspace to use.
set_atomspace(a)

# Create a truth value asserting true and mostly confident.
TV = TruthValue(1, 0.8)

# Add three nodes
# A = a.add_node(types.ConceptNode, 'Apple', TV)
# B = a.add_node(types.ConceptNode, 'Berry', TruthValue(0.5,1))
# C = a.add_node(types.ConceptNode, 'Comestible', TV)
A = ConceptNode('Apple', TV)
B = ConceptNode('Berry', TruthValue(0.5, 0.75))
C = ConceptNode('Comestible', TV)

# Add three inhertance links, asserting that apples are berries
# and that berries are edible.
# AB = a.add_link(types.InheritanceLink, [A, B], TV)
# BC = a.add_link(types.InheritanceLink, [B, C], TV)
# AC = a.add_link(types.InheritanceLink, [A, C])

AB = InheritanceLink(A, B, TV)
BC = InheritanceLink(B, C, TV)
AC = InheritanceLink(A, C)


print "The atomspace contains:\n\n", a.get_atoms_by_type(types.Atom)
