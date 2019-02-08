#! /usr/bin/env python
#
# create_atoms_simple.py
#
"""
A simple way of creating atoms in the AtomSpace.
See also create_stoms_by_type.py for an alternate API for atoms.
"""

from opencog.atomspace import AtomSpace
from opencog.atomspace import types
from opencog.type_constructors import *

a = AtomSpace()

# Tell the type constructors which atomspace to use.
set_type_ctor_atomspace(a)

# Create a truth value asserting true and mostly confident.
TV = TruthValue(1, 0.8)

# Add three nodes
A = ConceptNode('Apple', TV)
B = ConceptNode('Berry', TruthValue(0.5, 0.75))
C = ConceptNode('Comestible', TV)

# Add three inhertance links, asserting that apples are berries
# and that berries are edible.
AB = InheritanceLink(A, B, TV)
BC = InheritanceLink(B, C, TV)
AC = InheritanceLink(A, C)


print "The atomspace contains:\n\n", a.get_atoms_by_type(types.Atom)
