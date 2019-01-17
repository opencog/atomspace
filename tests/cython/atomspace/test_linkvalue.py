import unittest

from opencog.atomspace import AtomSpace
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
        value = LinkValue(StringSeqValue('foo'))
        self.assertTrue(value is not None)

    def test_create_list_value(self):
        value = LinkValue([FloatSeqValue(42), StringSeqValue('foo')])
        self.assertTrue(value is not None)
    
    def test_value_equals(self):
        self.assertEqual(LinkValue(StringSeqValue('foo')),
                         LinkValue([StringSeqValue('foo')]))
        self.assertEqual(LinkValue([FloatSeqValue(42), StringSeqValue('foo')]),
                         LinkValue([FloatSeqValue(42), StringSeqValue('foo')]))
        self.assertNotEqual(LinkValue(StringSeqValue('foo')),
                            LinkValue(StringSeqValue('bar')))
        self.assertNotEqual(LinkValue([FloatSeqValue(1), StringSeqValue('foo')]),
                            LinkValue([StringSeqValue('foo'), FloatSeqValue(1)]))

    def test_add_value_to_atom(self):
        atom = ConceptNode('foo')
        key = PredicateNode('bar')
        value = LinkValue([StringSeqValue('a'), FloatSeqValue(42)])
        atom.set_value(key, value)
        self.assertEqual(LinkValue([StringSeqValue('a'), FloatSeqValue(42)]),
                         atom.get_value(key))

    def test_get_list_of_items_from_value(self):
        value = LinkValue([FloatSeqValue(42), StringSeqValue('foo')])
        self.assertEqual([FloatSeqValue(42), StringSeqValue('foo')],
                         value.to_list())

    def test_str(self):
        value = LinkValue([FloatSeqValue(42), StringSeqValue('foo')])
        self.assertEqual('(LinkValue\n    (FloatSeqValue 42)\n    '
                         '(StringSeqValue "foo")\n)\n', str(value))

    def test_is_a(self):
        value = LinkValue([FloatSeqValue(42), StringSeqValue('foo')])
        self.assertEqual(types.LinkValue, value.type)
        self.assertEqual('LinkValue', value.type_name)
        self.assertFalse(value.is_node())
        self.assertFalse(value.is_atom())
        self.assertFalse(value.is_link())
        self.assertTrue(value.is_a(types.Value))
