#! /usr/bin/env python
#
# nameserver_example.py
#
"""
The AtomSpace contains a nameserver and a class factory. This
provides an abbreviated demo of how to use the nameserver to
walk the Type hierarchy.
"""

from opencog.atomspace import *
from opencog.type_constructors import *

# Tell the type constructors which atomspace to use.
set_default_atomspace(AtomSpace())

# A SetLink is of the type UnorderedLink. This can be checked directly.
set_is_unordered = is_a(get_type("SetLink"), get_type("UnorderedLink"))
print("Is a set unordered?", set_is_unordered)

# Is A a concept or a predicate?
A = ConceptNode("A")
print("Is A a concept?", is_a(A.type, get_type("ConceptNode")))
print("Is A a predicate?", is_a(A.type, get_type("PredicateNode")))
