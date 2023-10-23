#! /usr/bin/env python

"""
Checks the execution time of repeated calls to the Scheme API from Python

Runs an empty Scheme command NUMBER_OF_ITERATIONS times and displays the
total execution time
"""

__author__ = 'Cosmo Harrigan'

from opencog.atomspace import AtomSpace, TruthValue
from opencog.scheme_wrapper import scheme_eval, scheme_eval_h
atomspace = AtomSpace()

NUMBER_OF_ITERATIONS = 50000

def test_operation():
    for _ in range(NUMBER_OF_ITERATIONS):
        # scheme_eval(atomspace, '(+ 2 2)')
        scheme_eval_h(atomspace, '(Concept "foo")')
        # scheme_eval_h(atomspace, '(Number ' + str(i) + ')')

import timeit
elapsed = timeit.timeit("test_operation()",
                        setup="from __main__ import test_operation",
                        number=1)

def report():
    print ("{0} seconds elapsed performing {1} repeated calls = {2} calls / sec".\
        format(elapsed, NUMBER_OF_ITERATIONS, NUMBER_OF_ITERATIONS / elapsed))

report()
