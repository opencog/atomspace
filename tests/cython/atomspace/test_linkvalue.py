import unittest

from opencog.type_constructors import *
from opencog.utilities import initialize_opencog, finalize_opencog

class LinkValueTest(unittest.TestCase):
    
    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_create_single_value(self):
        value = LinkValue(StringValue('foo'))
        self.assertTrue(value is not None)

    def test_create_list_value(self):
        value = LinkValue([FloatValue(42), StringValue('foo')])
        self.assertTrue(value is not None)
    
    def test_value_equals(self):
        self.assertEqual(LinkValue(StringValue('foo')),
                         LinkValue([StringValue('foo')]))
        self.assertEqual(LinkValue([FloatValue(42), StringValue('foo')]),
                         LinkValue([FloatValue(42), StringValue('foo')]))
        self.assertNotEqual(LinkValue(StringValue('foo')),
                            LinkValue(StringValue('bar')))
        self.assertNotEqual(LinkValue([FloatValue(1), StringValue('foo')]),
                            LinkValue([StringValue('foo'), FloatValue(1)]))

    def test_add_value_to_atom(self):
        atom = ConceptNode('foo')
        key = PredicateNode('bar')
        value = LinkValue([StringValue('a'), FloatValue(42)])
        atom.set_value(key, value)
        self.assertEqual(LinkValue([StringValue('a'), FloatValue(42)]),
                         atom.get_value(key))

    def test_get_list_of_items_from_value(self):
        value = LinkValue([FloatValue(42), StringValue('foo')])
        self.assertEqual([FloatValue(42), StringValue('foo')],
                         value.to_list())

    def test_str(self):
        value = LinkValue([FloatValue(42), StringValue('foo')])
        self.assertEqual('(LinkValue\n  (FloatValue 42)\n  '
                         '(StringValue "foo"))', str(value))

    def test_is_a(self):
        value = LinkValue([FloatValue(42), StringValue('foo')])
        self.assertEqual(types.LinkValue, value.type)
        self.assertEqual('LinkValue', value.type_name)
        self.assertFalse(value.is_node())
        self.assertFalse(value.is_atom())
        self.assertFalse(value.is_link())
        self.assertTrue(value.is_a(types.Value))
