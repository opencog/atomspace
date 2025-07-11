import unittest

from opencog.type_constructors import *
from opencog.utilities import set_default_atomspace, finalize_opencog

class QueueValueTest(unittest.TestCase):
    
    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_create_empty(self):
        value = QueueValue()
        self.assertTrue(value is not None)
        self.assertEqual(0, len(value))

    def test_create_single_value(self):
        value = QueueValue(StringValue('foo'))
        self.assertTrue(value is not None)
        self.assertEqual(1, len(value))

    def test_create_list_value(self):
        value = QueueValue([FloatValue(42), StringValue('foo')])
        self.assertTrue(value is not None)
        self.assertEqual(2, len(value))
    
    def test_open_close(self):
        value = QueueValue()
        value.open()
        self.assertFalse(value.is_closed())
        value.close()
        self.assertTrue(value.is_closed())

    def test_push_pop(self):
        value = QueueValue()
        value.open()
        
        # Push some values
        value.push(FloatValue(1.0))
        value.push(StringValue('hello'))
        value.push(FloatValue(2.0))
        
        self.assertEqual(3, len(value))
        
        # Pop values - should be FIFO
        popped1 = value.pop()
        self.assertEqual(FloatValue(1.0), popped1)
        self.assertEqual(2, len(value))
        
        popped2 = value.pop()
        self.assertEqual(StringValue('hello'), popped2)
        self.assertEqual(1, len(value))
        
        popped3 = value.pop()
        self.assertEqual(FloatValue(2.0), popped3)
        self.assertEqual(0, len(value))
        
        value.close()

    def test_append(self):
        value = QueueValue()
        value.open()
        
        # Test append (should work like push)
        value.append(FloatValue(42))
        value.append(StringValue('test'))
        
        self.assertEqual(2, len(value))
        
        popped = value.pop()
        self.assertEqual(FloatValue(42), popped)
        
        value.close()

    def test_clear(self):
        value = QueueValue([FloatValue(1), StringValue('a'), FloatValue(2)])
        self.assertEqual(3, len(value))
        
        value.clear()
        self.assertEqual(0, len(value))

    def test_add_value_to_atom(self):
        atom = ConceptNode('foo')
        key = PredicateNode('bar')
        value = QueueValue([StringValue('a'), FloatValue(42)])
        atom.set_value(key, value)
        
        retrieved = atom.get_value(key)
        self.assertTrue(retrieved.is_a(types.QueueValue))
        self.assertEqual(2, len(retrieved))

    def test_get_list_of_items_from_value(self):
        value = QueueValue([FloatValue(42), StringValue('foo')])
        items = value.to_list()
        self.assertEqual([FloatValue(42), StringValue('foo')], items)

    def test_queue_with_atoms(self):
        value = QueueValue()
        value.open()
        
        # Add some atoms
        a1 = ConceptNode('concept1')
        a2 = ConceptNode('concept2')
        a3 = PredicateNode('predicate1')
        
        value.push(a1)
        value.push(a2)
        value.push(a3)
        
        self.assertEqual(3, len(value))
        
        # Pop and verify
        popped1 = value.pop()
        self.assertEqual(a1, popped1)
        self.assertTrue(popped1.is_node())
        
        value.close()

    def test_pop_empty_queue(self):
        value = QueueValue()
        value.open()
        
        # Popping from empty queue should return None
        popped = value.pop()
        self.assertIsNone(popped)
        
        value.close()

    def test_str(self):
        value = QueueValue([FloatValue(42), StringValue('foo')])
        str_repr = str(value)
        # QueueValue string representation should contain QueueValue
        self.assertIn('QueueValue', str_repr)

    def test_is_a(self):
        value = QueueValue([FloatValue(42), StringValue('foo')])
        self.assertEqual(types.QueueValue, value.type)
        self.assertEqual('QueueValue', value.type_name)
        self.assertFalse(value.is_node())
        self.assertFalse(value.is_atom())
        self.assertFalse(value.is_link())
        self.assertTrue(value.is_a(types.Value))
        self.assertTrue(value.is_a(types.QueueValue))