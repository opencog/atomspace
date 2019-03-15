import unittest
from unittest import TestCase

from opencog.atomspace import AtomSpace, Atom
from opencog.atomspace import types, is_a, get_type, get_type_name, create_child_atomspace

from opencog.type_constructors import *
from opencog.utilities import initialize_opencog, finalize_opencog

from time import sleep

class AtomTest(TestCase):

    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_get_value(self):
        atom = ConceptNode('foo')
        key = PredicateNode('bar')
        value = FloatValue([1.0, 2.0, 3.0])
        atom.set_value(key, value)
        self.assertEqual(value.__class__, atom.get_value(key).__class__)

    def test_get_out(self):
        atom = ListLink('list', ConceptNode('a'), ConceptNode('b'))

        out = atom.out

        self.assertEqual(out, [ConceptNode('a'), ConceptNode('b')])

    def test_get_input(self):
        atom = ConceptNode('node')
        a = ListLink(atom, ConceptNode('x'))
        b = ListLink(atom, ConceptNode('y'))

        incoming = atom.incoming

        self.assertEqual(set(incoming), set([a, b]))

if __name__ == '__main__':
    unittest.main()

