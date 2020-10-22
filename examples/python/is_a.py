#! /usr/bin/env python
#
# is_a.py
#
"""
A simple example of how to use the nameserver.
"""

from opencog.atomspace import AtomSpace
from opencog.atomspace import get_type, is_a
from opencog.atomspace import types
from opencog.type_constructors import *

a = AtomSpace()

# Tell the type constructors which atomspace to use.
set_default_atomspace(a)

# Is a set unordered
set_is_unordered = is_a(get_type("SetLink"), get_type("UnorderedLink"))
print("Is a set unordered?", set_is_unordered)

# Is A a concept or a predicate?
A = ConceptNode("A")
print("Is A a concept?", is_a(A.type, get_type("ConceptNode")))
print("Is A a predicate?", is_a(A.type, get_type("PredicateNode")))
