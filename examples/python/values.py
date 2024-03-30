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
print(f'{a} == {b}: {a == b}')
print(f'{a} == {c}: {a == c}')

featureValue = FloatValue([1.0, 2])
print(f'new value created: {featureValue}')

boundingBox = ConceptNode('boundingBox')
featureKey = PredicateNode('features')

boundingBox.set_value(featureKey, featureValue)
print(f'set value to atom: {boundingBox}')

value = boundingBox.get_value(featureKey)
print(f'get value from atom: {value}')

list = value.to_list()
print(f'get python list from value: {list}')

value = scheme_eval_v(atomspace, '(ValueOf (ConceptNode "boundingBox") '
                      '(PredicateNode "features"))')
value = boundingBox.get_value(featureKey)
print(f'get value from atom using Scheme program: {value}')
