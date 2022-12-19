import unittest
import re

from opencog.type_constructors import *
from opencog.utilities import initialize_opencog, finalize_opencog

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
        value = LinkValue([FloatValue(1), StringValue('foo'),
                           ConceptNode('bar')])
        self.assertTrue(value is not None)

    def test_value_equals(self):
        self.assertEqual(LinkValue(StringValue('foo')),
                         LinkValue([StringValue('foo')]))
        self.assertEqual(LinkValue([FloatValue(1), StringValue('foo'),
                                    ConceptNode('bar')]),
                         LinkValue([FloatValue(1), StringValue('foo'),
                                    ConceptNode('bar')]))
        self.assertNotEqual(LinkValue(FloatValue(1)),
                            LinkValue(FloatValue(2)))
        self.assertNotEqual(LinkValue([ConceptNode('bar'), FloatValue(1),
                                       StringValue('foo')]),
                            LinkValue([StringValue('foo'), FloatValue(1),
                                       ConceptNode('bar')]))

    def test_add_value_to_atom(self):
        atom = ConceptNode('foo')
        key = PredicateNode('bar')
        value = LinkValue([StringValue('a'), FloatValue(1),
                           ConceptNode('bar')])
        atom.set_value(key, value)
        self.assertEqual(LinkValue([StringValue('a'), FloatValue(1),
                                    ConceptNode('bar')]),
                         atom.get_value(key))

    def test_get_list_of_items_from_value(self):
        value = LinkValue([FloatValue(1), StringValue('foo'),
                           ConceptNode('bar')])
        self.assertEqual([FloatValue(1), StringValue('foo'),
                          ConceptNode('bar')], value.to_list())

    def test_str(self):
        value = LinkValue([FloatValue(1), StringValue('foo'),
                           ConceptNode('bar')])
        print(str(value))
        self.assertTrue(re.fullmatch(
            '\(LinkValue\n'
            '  \(FloatValue 1\)\n'
            '  \(StringValue "foo"\)\n'
            '  \(Concept "bar"\)\)',
            str(value)))

    def test_is_a(self):
        value = LinkValue([FloatValue(1), StringValue('foo'), 
                           ConceptNode('bar')])
        self.assertEqual(types.LinkValue, value.type)
        self.assertEqual('LinkValue', value.type_name)
        self.assertFalse(value.is_node())
        self.assertFalse(value.is_atom())
        self.assertFalse(value.is_link())
        self.assertTrue(value.is_a(types.Value))

if __name__ == '__main__':
    unittest.main()
