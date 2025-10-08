#
# my_py_func.py -- Python callback example.
#
# This is a short python snippet that is needed by the `execute.scm`
# example. The code below is called, when a GroundedSchemaNode is triggered.
#
from opencog.atomspace import AtomSpace
from opencog.atomspace import types
from opencog.type_constructors import *

asp = AtomSpace()

# Python function taking two atoms, converting their string-names to
# floats, adding the floats, and returning a new atom holding the sum.
def my_py_func(atoma, atomb):
    print('Python received two arguments:\n' + str(atoma) + str(atomb))
    av = float(atoma.name)
    bv = float(atomb.name)
    cv = av + bv       # Add numeric values!
    print(f'The sum is {str(cv)}')
    return asp.add_node(types.ConceptNode, str(cv))
