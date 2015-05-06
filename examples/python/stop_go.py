#! /usr/bin/env python
#
# stop_go.py
#
"""
Example of how to use the pattern matcher to callback into Python.
"""

from opencog.atomspace import AtomSpace, TruthValue, types, get_type_name
from opencog.bindlink import satisfaction_link
from opencog.type_constructors import *

atomspace = AtomSpace()
set_type_ctor_atomspace(atomspace)


green = 0
red = 0

def stop_go(atom):

    if atom == ConceptNode("green light"):
        print "Got a green light..."
        global green
        green += 1
        return TruthValue(1,1)

    elif atom == ConceptNode("red light"):
        print "Got a red light!"
        increment_red()
        global red
        red += 1
        return TruthValue(0,1)

    else:
        print "Oh No!! Car wreck!"
        assert(false)

    return TruthValue(0,0)



satisfaction_handle = SatisfactionLink(
    SequentialAndLink(
        EvaluationLink(
            GroundedPredicateNode("py: stop_go"),
            ListLink(
                ConceptNode("green light")
            )
        ),
        EvaluationLink(
            GroundedPredicateNode("py: stop_go"),
            ListLink(
                ConceptNode("green light")
            )
        ),
        EvaluationLink(
            GroundedPredicateNode("py: stop_go"),
            ListLink(
                ConceptNode("red light")
            )
        ),
        EvaluationLink(
            GroundedPredicateNode("py: stop_go"),
            ListLink(
                ConceptNode("traffic ticket")
            )
        )
    )
).h

result = satisfaction_link(atomspace, satisfaction_handle)

print "Number of green lights:",  green
print "Number of red lights:",  red
