#! /usr/bin/env python3
#
# create_atoms_by_type.py
#
"""
Example of how to use the low-level interfaces into the AtomSpace.

This provides the fastest python API for dumping raw data into the
AtomSpace. However, it is a bit hard to use; the Atomese-style
python is easier. See `create_atomspace_simple.py` for a simpler
variant.
"""

from opencog.atomspace import AtomSpace, Atom
from opencog.type_constructors import FloatValue, StringValue
from opencog.atomspace import types

a = AtomSpace()

# Add three Nodes
concept_type = types.ConceptNode
A = a.add_node(concept_type, 'Apple')
B = a.add_node(concept_type, 'Berry')
C = a.add_node(concept_type, 'Comestible')

# Create a Value holding a vector of numbers.
weights = FloatValue([1, 0.8, 42, 3.14159])

# Create a key
key = a.add_node(types.PredicateNode, 'my favorite keyname')

# Place some weights on the Nodes:
A.set_value(key, weights)
B.set_value(key, FloatValue([0.5, 0.333, 66, 2.71828]))
C.set_value(key, StringValue(["just", "some", "words"]))

# Get the weights:
A.get_value(key)
B.get_value(key)
C.get_value(key)

# Weights convert to ptyhon lists:
print("ello, there:", list(C.get_value(key)))

# Add three InheritanceLinks, asserting that apples are berries
# and that berries are edible.
inh_type = types.InheritanceLink
a.add_link(inh_type, [A, B])
a.add_link(inh_type, [B, C])
a.add_link(inh_type, [A, C])

print("The atomspace contains:\n\n", a.get_atoms_by_type(types.Atom))

# THE END. That's All, Folks!
