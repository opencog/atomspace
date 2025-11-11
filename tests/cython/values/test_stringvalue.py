import unittest

from opencog.type_constructors import *
from opencog.utilities import set_default_atomspace, finalize_opencog

class StringValueTest(unittest.TestCase):
    
    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_create_single_value(self):
        value = StringValue('foo')
        self.assertTrue(value is not None)

    def test_create_list_value(self):
        value = StringValue(['foo', 'bar'])
        self.assertTrue(value is not None)
    
    def test_value_equals(self):
        self.assertEqual(StringValue('foo'), StringValue(['foo']))
        self.assertEqual(StringValue(['foo', 'bar']),
                         StringValue(['foo', 'bar']))
        self.assertNotEqual(StringValue('foo'), StringValue('bar'))
        self.assertNotEqual(StringValue(['foo', 'bar']), 
                            StringValue(['bar', 'foo']))

    def test_add_value_to_atom(self):
        atom = ConceptNode('object')
        key = PredicateNode('predicate')
        value = StringValue(['foo', 'bar'])
        atom.set_value(key, value)
        self.assertEqual(StringValue(['foo', 'bar']), atom.get_value(key))

    def test_get_list_of_items_from_value(self):
        value = StringValue(['foo', 'bar'])
        self.assertEqual(['foo', 'bar'], value.to_list())

    def test_str(self):
        value = StringValue('foo')
        self.assertEqual('(StringValue "foo")', str(value))

    def test_is_a(self):
        value = StringValue('foo')
        self.assertEqual(types.StringValue, value.type)
        self.assertEqual('StringValue', value.type_name)
        self.assertFalse(value.is_node())
        self.assertFalse(value.is_atom())
        self.assertFalse(value.is_link())
        self.assertTrue(value.is_a(types.Value))
