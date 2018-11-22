import sys
from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types
from opencog.type_constructors import *
from opencog.bindlink import execute_atom

import mymodule as mm

a = AtomSpace()

set_type_ctor_atomspace(a)

def local_func(x1, x2, x3):
    print("Entering local function with\n", x1, x2, x3)
    return x3

class LocalClass:
    def forward(self, x1, x2):
        print("Entering LocalClass with\n", x1, x2)
        return x1

nn = LocalClass()

# local function
exlof = ExecutionOutputLink(
    GroundedSchemaNode("py:local_func"),
    ListLink(ConceptNode("aa"), ConceptNode("bb"), ConceptNode("cc")))

assert execute_atom(a, exlof) == ConceptNode("cc"), "Failed while calling local function"

# local object
exloc = ExecutionOutputLink(
    GroundedSchemaNode("py:nn.forward"),
    ListLink(ConceptNode("aa"), ConceptNode("bb")))

assert execute_atom(a, exloc) == ConceptNode("aa"), "Failed while calling local object method"

# external function
exexf = ExecutionOutputLink(
    GroundedSchemaNode("py:mm.mod_func"),
    ListLink(ConceptNode("aa"), ConceptNode("bb"), ConceptNode("cc")))

assert execute_atom(a, exexf) == ConceptNode("aa"), "Failed while calling external function"

# object in external module
exext = ExecutionOutputLink(
    GroundedSchemaNode("py:mm.nn.submodule.forward"),
    ListLink(ConceptNode("aa"), ConceptNode("bb")))

assert execute_atom(a, exext) == ConceptNode("bb"), "Failed while calling external object method"


'''
# also, here is an example of how to make a generic wrapper to call object methods
class LocalClass:
    def forward(self, listArgs):
        print("entering forward with args:")
        print(listArgs)
        return listArgs.out[0]
def callObjMethod(conceptObject, conceptFunction, listArgs):
    o = getattr(sys.modules[__name__], conceptObject.name)
    return getattr(o, conceptFunction.name)(listArgs)
ex = ExecutionOutputLink(
   GroundedSchemaNode("py:callObjMethod"),
   ListLink(ConceptNode("nn"),
            ListLink(ConceptNode("bb"))))
'''
