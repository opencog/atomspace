#
# Test Functions

from opencog.atomspace import types, Atom
from opencog.type_constructors import *


def print_arguments(argOne, argTwo):
    print ("argOne = ", argOne)
    print ("argTwo = ", argTwo)
    atomspace = argOne.atomspace
    atom = atomspace.add_link(types.ListLink, [argOne, argTwo])
    print ("atom = ", atom)
    return atom

def add_link(atom_one, atom_two):
    return atom_one.atomspace.add_link(types.ListLink, [atom_one, atom_two])

def bogus_tv(atom_one, atom_two):
    return TruthValue(0.6, 0.234)

green = 0
red = 0

def initialize_counts():
    global red
    global green
    green = 0
    red = 0

def increment_green():
    global green
    green += 1

def increment_red():
    global red
    red += 1

def green_count():
    global green
    return green

def red_count():
    global red
    return red

def stop_go(atom):
    compare_green = ConceptNode("green light")
    compare_red = ConceptNode("red light")
    if atom == compare_green:
        print ("got a green light!")
        increment_green()
        return TruthValue(1,1)
    elif atom == compare_red:
        print ("got a red light")
        increment_red()
        return TruthValue(0,1)
    else:
        print ("got no match :-(")
        assert(false)

    return TruthValue(0,0)

func_one_result = TruthValue (0,1) # false
def func_one(v):
    thing = ConceptNode("barleycorn")
    thang = ListLink(thing, v)
    thang.tv = func_one_result
    return thang.tv


def func_two(v):
    return ListLink(ConceptNode("barleycorn"), v).tv
