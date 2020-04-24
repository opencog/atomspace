import unittest

from opencog.type_constructors import *
from opencog.utilities import initialize_opencog, finalize_opencog

class FloatValueTest(unittest.TestCase):
    
    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_create_single_value(self):
        value = FloatValue(1.234)
        self.assertTrue(value is not None)

    def test_create_list_value(self):
        value = FloatValue([3.21, 2.1, 1])
        self.assertTrue(value is not None)
    
    def test_value_equals(self):
        self.assertEqual(FloatValue(1.234), FloatValue([1.234]))
        self.assertEqual(FloatValue([1.0, 2.0, 3.0]),
                         FloatValue([1.0, 2.0 ,3.0]))
        self.assertNotEqual(FloatValue(1.0), FloatValue(2.0))
        self.assertNotEqual(FloatValue(1), FloatValue(2))
        self.assertNotEqual(FloatValue([1.0, 2.0, 3.0]), 
                            FloatValue([3.0, 2.0, 1.0]))

    def test_add_value_to_atom(self):
        atom = ConceptNode('foo')
        key = PredicateNode('bar')
        value = FloatValue([1.0, 2.0, 3.0])
        atom.set_value(key, value)
        self.assertEqual(FloatValue([1.0, 2.0, 3.0]), atom.get_value(key))

    def test_get_list_of_items_from_value(self):
        value = FloatValue([1.0, 2.0, 3.0])
        self.assertEqual([1.0, 2.0, 3.0], value.to_list())

    def test_str(self):
        value = FloatValue(1.234)
        self.assertEqual('(FloatValue 1.234)', str(value))

    def test_is_a(self):
        value = FloatValue(1.234)
        self.assertEqual(types.FloatValue, value.type)
        self.assertEqual('FloatValue', value.type_name)
        self.assertFalse(value.is_node())
        self.assertFalse(value.is_atom())
        self.assertFalse(value.is_link())
        self.assertTrue(value.is_a(types.Value))
