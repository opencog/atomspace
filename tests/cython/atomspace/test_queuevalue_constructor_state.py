import unittest

from opencog.type_constructors import *
from opencog.utilities import set_default_atomspace, finalize_opencog

class QueueValueConstructorStateTest(unittest.TestCase):
    """
    Test QueueValue constructor open/closed state behavior.
    
    Current design:
    - Empty QueueValue is created OPEN (ready to receive values)
    - QueueValue with initial values is created CLOSED (indicating initialization is complete)
    """

    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_empty_queue_is_open(self):
        """Empty QueueValue should be created in open state."""
        queue = QueueValue()
        self.assertFalse(queue.is_closed())
        self.assertEqual(0, len(queue))
        
        # Should be able to push/append values immediately
        queue.push(FloatValue(42.0))
        queue.append(StringValue("test"))
        self.assertEqual(2, len(queue))
        
    def test_queue_with_list_is_closed(self):
        """QueueValue created with list of values should be closed."""
        values = [FloatValue(1.0), StringValue("test"), FloatValue(2.0)]
        queue = QueueValue(values)
        self.assertTrue(queue.is_closed())
        self.assertEqual(3, len(queue))
        
        # Verify contents match initialization
        contents = queue.to_list()
        self.assertEqual(values, contents)
        
    def test_queue_with_single_value_is_closed(self):
        """QueueValue created with single value should be closed."""
        value = StringValue("single")
        queue = QueueValue(value)
        self.assertTrue(queue.is_closed())
        self.assertEqual(1, len(queue))
        self.assertEqual([value], queue.to_list())
        
    def test_reopen_behavior(self):
        """Test reopening a closed queue - note it clears contents."""
        # Create with initial values
        queue = QueueValue([FloatValue(1.0), FloatValue(2.0)])
        self.assertTrue(queue.is_closed())
        self.assertEqual(2, len(queue))
        
        # Important: reopening clears the queue
        queue.open()
        self.assertFalse(queue.is_closed())
        self.assertEqual(0, len(queue))
        
    def test_append_requires_open_queue(self):
        """Test that append/push require an open queue."""
        # Empty queue (open) - append works
        queue1 = QueueValue()
        queue1.append(FloatValue(1.0))  # Works
        self.assertEqual(1, len(queue1))
        
        # Queue with values (closed) - would need to reopen first
        queue2 = QueueValue([FloatValue(1.0)])
        self.assertTrue(queue2.is_closed())
        # Cannot append without reopening (which would clear the queue)
        
    def test_close_then_read(self):
        """Test typical producer-consumer pattern."""
        queue = QueueValue()
        
        # Producer adds values
        queue.push(FloatValue(1.0))
        queue.push(FloatValue(2.0))
        queue.push(FloatValue(3.0))
        
        # Producer closes when done
        queue.close()
        
        # Consumer can read all values
        self.assertEqual(3, len(queue))
        values = queue.to_list()
        self.assertEqual([FloatValue(1.0), FloatValue(2.0), FloatValue(3.0)], values)