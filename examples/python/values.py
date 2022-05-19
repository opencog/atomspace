#! /usr/bin/env python
#
# values.py
#
"""
An example of using values via Python API
"""

from opencog.atomspace import AtomSpace, TruthValue
from opencog.type_constructors import *
from opencog.scheme_wrapper import scheme_eval_v

atomspace = AtomSpace()
set_default_atomspace(atomspace)

a = FloatValue([1.0, 2.0, 3.0])
b = FloatValue([1.0, 2.0, 3.0])
c = FloatValue(1.0)
print('{} == {}: {}'.format(a, b, a == b))
print('{} == {}: {}'.format(a, c, a == c))

featureValue = FloatValue([1.0, 2])
print('new value created: {}'.format(featureValue))

boundingBox = ConceptNode('boundingBox')
featureKey = PredicateNode('features')

boundingBox.set_value(featureKey, featureValue)
print('set value to atom: {}'.format(boundingBox))

value = boundingBox.get_value(featureKey)
print('get value from atom: {}'.format(value))

list = value.to_list()
print('get python list from value: {}'.format(list))

value = scheme_eval_v(atomspace, '(ValueOf (ConceptNode "boundingBox") '
                      '(PredicateNode "features"))')
value = boundingBox.get_value(featureKey)
print('get value from atom using Scheme program: {}'.format(value))
