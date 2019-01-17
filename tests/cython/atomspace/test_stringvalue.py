import unittest

from opencog.atomspace import AtomSpace
from opencog.type_constructors import *
from opencog.utilities import initialize_opencog, finalize_opencog

class StringSeqValueTest(unittest.TestCase):
    
    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_create_single_value(self):
        value = StringSeqValue('foo')
        self.assertTrue(value is not None)

    def test_create_list_value(self):
        value = StringSeqValue(['foo', 'bar'])
        self.assertTrue(value is not None)
    
    def test_value_equals(self):
        self.assertEqual(StringSeqValue('foo'), StringSeqValue(['foo']))
        self.assertEqual(StringSeqValue(['foo', 'bar']),
                         StringSeqValue(['foo', 'bar']))
        self.assertNotEqual(StringSeqValue('foo'), StringSeqValue('bar'))
        self.assertNotEqual(StringSeqValue(['foo', 'bar']), 
                            StringSeqValue(['bar', 'foo']))

    def test_add_value_to_atom(self):
        atom = ConceptNode('object')
        key = PredicateNode('predicate')
        value = StringSeqValue(['foo', 'bar'])
        atom.set_value(key, value)
        self.assertEqual(StringSeqValue(['foo', 'bar']), atom.get_value(key))

    def test_get_list_of_items_from_value(self):
        value = StringSeqValue(['foo', 'bar'])
        self.assertEqual(['foo', 'bar'], value.to_list())

    def test_str(self):
        value = StringSeqValue('foo')
        self.assertEqual('(StringSeqValue "foo")\n', str(value))

    def test_is_a(self):
        value = StringSeqValue('foo')
        self.assertEqual(types.StringSeqValue, value.type)
        self.assertEqual('StringSeqValue', value.type_name)
        self.assertFalse(value.is_node())
        self.assertFalse(value.is_atom())
        self.assertFalse(value.is_link())
        self.assertTrue(value.is_a(types.Value))
