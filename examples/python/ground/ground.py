#! /usr/bin/env python3
#
# ground.py
#
"""
Demo of calling python from Atomese.
Both regular function calls, and class instance method calls are demoed.
"""

import sys
from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types
from opencog.type_constructors import *
from opencog.exec import execute_atom

import mymodule as mm

a = AtomSpace()

set_default_atomspace(a)

def local_func(x1, x2, x3):
    print("Entering local function with\n", x1, x2, x3)
    return x3

class LocalClass:
    @staticmethod
    def static_check(x1):
        print("Entering static method with\n", x1)
        return x1
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

# static method
exst = ExecutionOutputLink(
    GroundedSchemaNode("py:LocalClass.static_check"),
    ListLink(ConceptNode("aa")))

assert execute_atom(a, exst) == ConceptNode("aa"), "Failed while calling static class method"

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
