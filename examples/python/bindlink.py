#! /usr/bin/env python
#
# bindlink.py
#
"""
Example of how to use the pattern matcher.
Based on the following example in the wiki:
http://wiki.opencog.org/w/Pattern_matching#The_Simplified_API
"""

__author__ = 'Cosmo Harrigan'

from opencog.atomspace import AtomSpace
from opencog.scheme_wrapper import load_scm, scheme_eval, scheme_eval_h

atomspace = AtomSpace()

data = ["opencog/atomspace/core_types.scm",
        "opencog/scm/utilities.scm"]

for item in data:
    load_scm(atomspace, item)

# Add to scheme's %load-path directory to serach for opencog guile modules
scheme_eval(atomspace, "(add-to-load-path \"/usr/local/share/opencog/scm\")")

# Import opencog modules required for using `cog-bind` in scheme_eval
scheme_eval(atomspace, "(use-modules (opencog))")
scheme_eval(atomspace, "(use-modules (opencog query))")

# Define several animals and something of a different type as well
scheme_animals = \
    '''
    (InheritanceLink (ConceptNode "Frog") (ConceptNode "animal"))
    (InheritanceLink (ConceptNode "Zebra") (ConceptNode "animal"))
    (InheritanceLink (ConceptNode "Deer") (ConceptNode "animal"))
    (InheritanceLink (ConceptNode "Spaceship") (ConceptNode "machine"))
    '''
scheme_eval_h(atomspace, scheme_animals)
# Define a graph search query
scheme_query = \
    '''
    (define find-animals
      (BindLink
        ;; The variable to be bound
        (VariableNode "$var")

        ;; The pattern to be searched for
        (InheritanceLink
           (VariableNode "$var")
           (ConceptNode "animal")
        )

        ;; The value to be returned.
        (VariableNode "$var")
      )
    )
    '''
scheme_eval_h(atomspace, scheme_query)

# Run the above pattern and print the result
result = scheme_eval_h(atomspace, '(cog-bind find-animals)')
print "The result of pattern matching is:\n\n" + str(atomspace[result])
