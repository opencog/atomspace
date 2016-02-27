#
# A short python snippet that is used in the execute.scm example.
#
from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types

asp = AtomSpace()

def my_py_func(atoma, atomb):
	print("Python received two arguments:\n" + str(atoma) + str(atomb))
	av = float(atoma.name)
	bv = float(atomb.name)
	cv = av + bv       # Add numeric values!
	return asp.add_node(types.ConceptNode, str(cv))
