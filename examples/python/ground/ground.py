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
exlof = ExecutionOutput(
    GroundedSchema("py:local_func"),
    ListLink(Concept("aa"), Concept("bb"), Concept("cc")))

assert exlof.execute() == Concept("cc"), "Failed while calling local function"

# local object
exloc = ExecutionOutput(
    GroundedSchema("py:nn.forward"),
    List(Concept("aa"), Concept("bb")))

assert exloc.execute() == Concept("aa"), "Failed while calling local object method"

# static method
exst = ExecutionOutput(
    GroundedSchema("py:LocalClass.static_check"),
    ListLink(Concept("aa")))

assert exst.execute() == Concept("aa"), "Failed while calling static class method"

# external function
exexf = ExecutionOutput(
    GroundedSchema("py:mm.mod_func"),
    ListLink(Concept("aa"), Concept("bb"), Concept("cc")))

assert exexf.execute() == Concept("aa"), "Failed while calling external function"

# object in external module
exext = ExecutionOutputLink(
    GroundedSchemaNode("py:mm.nn.submodule.forward"),
    ListLink(ConceptNode("aa"), ConceptNode("bb")))

assert exext.execute() == Concept("bb"), "Failed while calling external object method"


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

ex = ExecutionOutput(
   GroundedSchemaNode("py:callObjMethod"),
   List(Concept("nn"),
            List(Concept("bb"))))
'''
