#! /usr/bin/env python3
#
# chemy_hello.py
#
# An example of using the Python bindings to the demo types.
#
# --------------------------------------------------------------

# Import the AtomSpace, and the basic AtomSpace types
from opencog.atomspace import AtomSpace
from opencog.type_constructors import *

# Import all of the chemical element types, and bond types too.
from opencog.chempydemo import *

# Nothing works without a default AtomSpace, so create that first.
spa = AtomSpace()
set_default_atomspace(spa)

print ('Hello! The AtomSpace is ' + str(spa))

# Lets create a Magnesium atom named 'foo'
Mg('foo')

# Uhh, well, lets grab hold of it
define foomg Mg('foo')

print ('The Magnesium atom is ' + str(foomg))

# Let's create a single bond between a Carbon and a Hydrogen
# The atoms must have names. The names can be anything.
SB(C('some carbon atom'), H('just a proton, ok?'))

# Let's create a methane molecule. The carbon and the hydrogens will all
# be numbered.
Molecule( \
  SB(C('1'), H('1')), \
  SB(C('1'), H('2')), \
  SB(C('1'), H('3')), \
  SB(C('1'), H('4')))

# The end.
# That's all, folks!
