#! /usr/bin/env python
#
# is_a.py
#
"""
A simple example of how to use the nameserver.
"""

from opencog.atomspace import get_type, is_a

# Is a set unordered
set_is_unordered = is_a(get_type("SetLink"), get_type("UnorderedLink"))
print("Is a set unordered?", set_is_unordered)
