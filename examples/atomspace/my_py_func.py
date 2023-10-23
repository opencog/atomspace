#
# my_py_func.py -- Python callback example.
#
# This is a short python snippet that is needed by the `execute.scm`
# example. The code below is called, when a GroundedSchemaNode and
# a GroundedPredicateNode is triggered.
#
from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types

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

# Similar to the above, but returning a truth value.
def my_py_predicate(atoma, atomb):
    print('Python predicate received two arguments:\n' + str(atoma) + str(atomb))
    av = float(atoma.name)
    bv = float(atomb.name)
    return TruthValue(1.0/av, 1.0/bv)
