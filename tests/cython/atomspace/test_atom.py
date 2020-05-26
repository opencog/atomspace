import unittest
from unittest import TestCase

from opencog.atomspace import Atom
from opencog.execute import execute_atom

from opencog.atomspace import types, is_a, get_type, get_type_name, create_child_atomspace

from opencog.type_constructors import *
from opencog.utilities import push_default_atomspace, pop_default_atomspace

from time import sleep

class AtomTest(TestCase):

    def setUp(self):
        self.space = AtomSpace()
        push_default_atomspace(self.space)

    def tearDown(self):
        self.space = None
        pop_default_atomspace()

    def test_get_value(self):
        atom = ConceptNode('foo')
        key = PredicateNode('bar')
        value = FloatValue([1.0, 2.0, 3.0])
        atom.set_value(key, value)
        self.assertEqual(value, atom.get_value(key))

    def test_get_keys(self):
        atom = ConceptNode('foo')
        keys = atom.get_keys()
        self.assertEqual(0, len(keys))

        tv = TruthValue(0.7, 0.7)
        atom.tv = tv
        keys = atom.get_keys()
        self.assertEqual(1, len(keys))
        # Since the type or name of the TruthValue key may change, check that
        # the value it referes to is the same.
        self.assertEqual(tv, atom.get_value(keys[0]))

        key = PredicateNode('bar')
        value = FloatValue([1.0, 2.0, 3.0])
        atom.set_value(key, value)
        keys = atom.get_keys()
        self.assertEqual(2, len(keys))
        self.assertIn(key, keys)

    def test_get_out(self):

        with self.assertRaises(TypeError):
            atom = ListLink('list', ConceptNode('a'), ConceptNode('b'))

        atom = ListLink(ConceptNode('a'), ConceptNode('b'))
        out = atom.out

        self.assertEqual(out, [ConceptNode('a'), ConceptNode('b')])

    def test_get_input(self):
        atom = ConceptNode('node')
        a = ListLink(atom, ConceptNode('x'))
        b = ListLink(atom, ConceptNode('y'))

        incoming = atom.incoming

        self.assertEqual(set(incoming), set([a, b]))

    def test_invalid_key(self):
        string_node = ConceptNode("String")
        error_str = "key should be an instance of Atom, got {0} instead".format(str)
        with self.assertRaisesRegex(TypeError, error_str):
            string_node.set_value("bad key", StringValue("Hello, World!"))

    def test_grounded_cond(self):
        grounded_cond = CondLink(
                    EvaluationLink (
                        GroundedPredicateNode ("py:grounded_cond1"),
                        ListLink ()),
                    NumberNode('1'),
                        EvaluationLink(
                            GroundedPredicateNode("py:grounded_cond2"),
                            ListLink()),
                    NumberNode('2'))
        result = execute_atom(self.space, grounded_cond)
        baz = NumberNode("2")
        print("got %s", result)
        print("expected %s\n", baz)
        self.assertTrue(result == baz)


def grounded_cond1(*args):
    print(args)
    return TruthValue(0, 0)

def grounded_cond2(*args):
    print(args)
    return TruthValue(1, 1)

import __main__

__main__.grounded_cond1 = grounded_cond1
__main__.grounded_cond2 = grounded_cond2


if __name__ == '__main__':
    unittest.main()

