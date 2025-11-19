#
# Test Functions

from opencog.atomspace import types, Atom, tvkey
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
    return True

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
        return True
    elif atom == compare_red:
        print ("got a red light")
        increment_red()
        return False
    else:
        assert(false)

    return False

func_one_result = FloatValue([0,0,1])
def func_one(v):
    thing = ConceptNode("barleycorn")
    thang = ListLink(thing, v)
    thang = thang.atomspace.set_value(thang, tvkey, func_one_result)
    print(f"func_one: set {func_one_result} on {thang}")
    if (func_one_result == FloatValue([0,0,1])):
        return False
    return True


def func_two(v):
    tv = ListLink(ConceptNode("barleycorn"), v).get_value(tvkey)
    if (tv is None):
        return True
    if (tv == FloatValue([1,1,1])):
        return True
    return False
