import unittest

from opencog.atomspace import AtomSpace
from opencog.type_constructors import *
from opencog.utilities import initialize_opencog, finalize_opencog

@unittest.skip('Not implemented, see comment to '
               'vector_of_values_to_list()')
class LinkValueContainingAtomsTest(unittest.TestCase):
    
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
        value = LinkValue([FloatSeqValue(1), StringValue('foo'),
                           ConceptNode('bar')])
        self.assertTrue(value is not None)
    
    def test_value_equals(self):
        self.assertEqual(LinkValue(StringValue('foo')),
                         LinkValue([StringValue('foo')]))
        self.assertEqual(LinkValue([FloatSeqValue(1), StringValue('foo'),
                                    ConceptNode('bar')]),
                         LinkValue([FloatSeqValue(1), StringValue('foo'),
                                    ConceptNode('bar')]))
        self.assertNotEqual(LinkValue(FloatSeqValue(1)),
                            LinkValue(FloatSeqValue(2)))
        self.assertNotEqual(LinkValue([ConceptNode('bar'), FloatSeqValue(1),
                                       StringValue('foo')]),
                            LinkValue([StringValue('foo'), FloatSeqValue(1),
                                       ConceptNode('bar')]))

    def test_add_value_to_atom(self):
        atom = ConceptNode('foo')
        key = PredicateNode('bar')
        value = LinkValue([StringValue('a'), FloatSeqValue(1),
                           ConceptNode('bar')])
        atom.set_value(key, value)
        self.assertEqual(LinkValue([StringValue('a'), FloatSeqValue(1),
                                    ConceptNode('bar')]),
                         atom.get_value(key))

    def test_get_list_of_items_from_value(self):
        value = LinkValue([FloatSeqValue(1), StringValue('foo'),
                           ConceptNode('bar')])
        self.assertEqual([FloatSeqValue(1), StringValue('foo'),
                          ConceptNode('bar')], value.to_list())

    def test_str(self):
        value = LinkValue([FloatSeqValue(1), StringValue('foo'),
                           ConceptNode('bar')])
        self.assertEqual('(LinkValue\n    (FloatSeqValue 42)\n    '
                         '(StringValue "foo")\n    (ConceptNode "bar")\n)\n',
                         str(value))

    def test_is_a(self):
        value = LinkValue([FloatSeqValue(1), StringValue('foo'), 
                           ConceptNode('bar')])
        self.assertEqual(types.LinkValue, value.type)
        self.assertEqual('LinkValue', value.type_name)
        self.assertFalse(value.is_node())
        self.assertFalse(value.is_atom())
        self.assertFalse(value.is_link())
        self.assertTrue(value.is_a(types.Value))

