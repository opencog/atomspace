import unittest

from opencog.type_constructors import *
from opencog.utilities import set_default_atomspace, finalize_opencog

class BoolValueTest(unittest.TestCase):

    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_create_single_value(self):
        value = BoolValue(True)
        self.assertTrue(value is not None)

    def test_create_list_value(self):
        value = BoolValue([False, True, True])
        self.assertTrue(value is not None)

    def test_value_equals(self):
        self.assertEqual(BoolValue(False), BoolValue([False]))
        self.assertEqual(BoolValue(True), BoolValue([True]))
        self.assertEqual(BoolValue([False, True, False]),
                         BoolValue([False, True ,False]))
        self.assertNotEqual(BoolValue(False), BoolValue(True))
        self.assertNotEqual(BoolValue(True), BoolValue([False]))
        self.assertNotEqual(BoolValue([False, True, True]),
                            BoolValue([False, True, False]))

    def test_add_value_to_atom(self):
        atom = ConceptNode('foo')
        key = PredicateNode('bar')
        value = BoolValue([False, True, False])
        atom.set_value(key, value)
        self.assertEqual(BoolValue([False, True, False]), atom.get_value(key))

    def test_get_list_of_items_from_value(self):
        value = BoolValue([False, True, False])
        self.assertEqual([False, True, False], value.to_list())

    def test_str(self):
        value = BoolValue(True)
        self.assertEqual('(BoolValue 1)', str(value))

    def test_is_a(self):
        value = BoolValue(True)
        self.assertEqual(types.BoolValue, value.type)
        self.assertEqual('BoolValue', value.type_name)
        self.assertFalse(value.is_node())
        self.assertFalse(value.is_atom())
        self.assertFalse(value.is_link())
        self.assertTrue(value.is_a(types.Value))
