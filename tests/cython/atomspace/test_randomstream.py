import unittest

from opencog.atomspace import AtomSpace
from opencog.type_constructors import *
from opencog.utilities import initialize_opencog, finalize_opencog

class RandomStreamTest(unittest.TestCase):
    
    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_create_value(self):
        value = RandomStream(3)
        self.assertTrue(value is not None)

    def test_value_equals(self):
        a = RandomStream(3)
        b = RandomStream(3)
        self.assertEqual(a, a)
        self.assertNotEqual(a, b)

    def test_add_value_to_atom(self):
        atom = ConceptNode('foo')
        key = PredicateNode('bar')
        value = RandomStream(3)
        atom.set_value(key, value)
        self.assertEqual(value, atom.get_value(key))

    def test_get_list_of_items_from_value(self):
        value = RandomStream(3)
        self.assertEqual(3, len(value.to_list()))

    def test_str(self):
        value = RandomStream(3)
        self.assertEqual('(RandomStream 3)\n', str(value))

    def test_is_a(self):
        value = RandomStream(3)
        self.assertEqual(types.RandomStream, value.type)
        self.assertEqual('RandomStream', value.type_name)
        self.assertFalse(value.is_node())
        self.assertFalse(value.is_atom())
        self.assertFalse(value.is_link())
        self.assertTrue(value.is_a(types.Value))
