import unittest
from opencog.atomspace import AtomSpace
from opencog.type_constructors import *


class TestQueueValueExceptions(unittest.TestCase):
    def setUp(self):
        self.atomspace = AtomSpace()

    def tearDown(self):
        del self.atomspace

    def test_push_to_closed_queue_raises_exception(self):
        """Test that pushing to a closed queue raises a Python exception."""
        q = QueueValue()
        q.close()
        
        # This should raise a Python exception, not crash with C++ exception
        with self.assertRaises(RuntimeError) as context:
            q.push(FloatValue(3.14))
        
        # The C++ exception is converted to RuntimeError with message "std::exception"
        self.assertEqual(str(context.exception), "std::exception")

    def test_append_to_closed_queue_raises_exception(self):
        """Test that appending to a closed queue raises a Python exception."""
        q = QueueValue()
        q.close()
        
        # append() calls push() internally, so should also raise
        with self.assertRaises(RuntimeError) as context:
            q.append(StringValue("test"))
        
        # The C++ exception is converted to RuntimeError with message "std::exception"
        self.assertEqual(str(context.exception), "std::exception")

    def test_push_to_open_queue_succeeds(self):
        """Test that pushing to an open queue works correctly."""
        q = QueueValue()
        q.open()
        
        # These should succeed without exceptions
        try:
            q.push(FloatValue(1.0))
            q.push(StringValue("hello"))
            q.push(FloatValue(2.0))
        except Exception as e:
            self.fail(f"Pushing to open queue raised unexpected exception: {e}")
        
        q.close()
        self.assertEqual(len(q), 3)

    def test_queue_with_initial_values_push_behavior(self):
        """Test pushing to a queue created with initial values."""
        # Queue created with initial values is automatically closed
        q = QueueValue([FloatValue(1.0), StringValue("initial")])
        
        # Should raise exception when trying to push
        with self.assertRaises(RuntimeError) as context:
            q.push(FloatValue(2.0))
        
        self.assertEqual(str(context.exception), "std::exception")
        
        # But we can reopen it
        q.open()
        q.push(FloatValue(3.0))
        q.close()
        
        self.assertEqual(len(q), 3)

    def test_pop_from_closed_empty_queue(self):
        """Test that popping from closed empty queue returns VoidValue."""
        q = QueueValue()
        q.open()
        q.close()

        # This should return empty value instead of raising exception
        result = q.pop()
        self.assertEqual(0, len(result.to_list()),
                        "Expected empty value when popping from closed empty queue")


if __name__ == '__main__':
    unittest.main()
