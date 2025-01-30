#! /usr/bin/env python3
#
# create_atoms.py
#
"""
If you plan to hand-write a bunch of Atomese, then there is a simpler
and easier API for this: just use the Atom type-names directly.
This is not quite as CPU-efficient as the low level API demoed in
`create_atoms_lowlevel.py`, but is a lot easier to read.
"""

from opencog.atomspace import *
from opencog.type_constructors import *

# Create an AtomSpace, and tell the type constructors to use it.
set_default_atomspace(AtomSpace())

# Add three nodes
A = ConceptNode('Apple')
B = ConceptNode('Berry')
C = ConceptNode('Comestible')

# Create a Value holding a vector of numbers.
weights = FloatValue([1, 0.8, 42, 3.14159])

# Create a key
key = PredicateNode('my favorite keyname')

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
InheritanceLink(A, B)
InheritanceLink(B, C)
InheritanceLink(A, C)

# InheritanceLinks are just sugar-coating on generic graph edge types.
# For example:
EdgeLink(PredicateNode("is a"), ListLink(A, ConceptNode("fruit")))
EdgeLink(PredicateNode("has a"), ListLink(A, ConceptNode("stem")))
is_red = EdgeLink(PredicateNode("often colored"), ListLink(A, ConceptNode("red")))

# But only half the time:
is_red.set_value(key, FloatValue(0.5))

# Although set_value() works, it is a proceedural call, and should
# only be used under duress. The correct Atomse fashion is to create
# a graph, that, when executed, sets the weight.
#
# Here's the graph:
setter_graph = SetValueLink(is_red, key, NumberNode(0.777))

# Note that the weight has not yet been set:
is_red.get_value(key)

# Now execute the code described in the graph:
setter_graph.execute()

# The weight has changed:
is_red.get_value(key)

# Or, better yet, in a declarative style:
ValueOfLink(is_red, key).execute()

# MeetLink is a type of query. It is named after the joins and meets of
# a poset. The AtomSpace is a poset, and meeting at the top will print
# the entire contents of the AtomSpace. Queries are demoed in later
# files. This is the simplest demo.
print("The atomspace contains:\n\n",
   MeetLink(VariableNode("x")).execute())

# THE END. That's All, Folks!
