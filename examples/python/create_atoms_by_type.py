#! /usr/bin/env python3
#
# create_atoms_by_type.py
#
"""
Simple example of how to create atoms in the AtomSpace.
See also create_atomspace_simple.py for an alternate interface.
"""

from opencog.atomspace import AtomSpace, Atom
from opencog.type_constructors import TruthValue
from opencog.atomspace import types

a = AtomSpace()

# Create a truth value asserting true and mostly confident.
TV = TruthValue(1, 0.8)

# Add three nodes
concept_type = types.ConceptNode
A = a.add_node(concept_type, 'Apple', TV)
B = a.add_node(concept_type, 'Berry', TruthValue(0.5, 0.75))
C = a.add_node(concept_type, 'Comestible', TV)

# Add three inheritance links, asserting that apples are berries
# and that berries are edible.
inh_type = types.InheritanceLink
AB = a.add_link(inh_type, [A, B], TV)
BC = a.add_link(inh_type, [B, C], TV)
AC = a.add_link(inh_type, [A, C])


print("The atomspace contains:\n\n", a.get_atoms_by_type(types.Atom))
