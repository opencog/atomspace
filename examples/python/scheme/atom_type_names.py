#! /usr/bin/env python
#
# atom_type_names.py
#
"""
Example of how to obtain atom type names and atom type IDs in Python
"""

__author__ = 'Cosmo Harrigan'

from opencog.atomspace import AtomSpace, types, get_type_name

atomspace = AtomSpace()

atom = atomspace.add(types.ConceptNode, "Frog #1")

# To get one type name
print(get_type_name(3) + '\n')

# To get one atom's type name
print(get_type_name(atom.type) + '\n')

# Get a list of all possible type names and numbers
for key, value in sorted(types.__dict__.items()):
    if '__' not in key:
        print(key, value)
