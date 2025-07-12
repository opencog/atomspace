import unittest
import threading
import time
import random

from opencog.type_constructors import *
from opencog.utilities import set_default_atomspace, finalize_opencog


class UnisetValueMultithreadTest(unittest.TestCase):
    """Test concurrent operations on UnisetValue to ensure thread safety."""

    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_concurrent_producer_consumer(self):
        """Test concurrent producer-consumer pattern with multiple threads."""
        uniset = UnisetValue()
        uniset.open()

        # Shared state
        produced_values = set()  # Use set to track unique values
        consumed_values = []
        producer_exception = None
        consumer_exception = None

        # Number of items to produce/consume
        num_items = 10000

        def producer():
            """Producer thread that writes values to the set."""
            nonlocal producer_exception
            try:
                random.seed(42)  # For reproducibility
                for i in range(num_items):
                    # Create different types of values
                    if i % 4 == 0:
                        # Float values
                        val = FloatValue(float(i))
                    elif i % 4 == 1:
                        # String values
                        val = StringValue(f'string_{i}')
                    elif i % 4 == 2:
                        # More FloatValues
                        val = FloatValue(float(i * 2))
                    else:
                        # More StringValues
                        val = StringValue(f'value_{i}')

                    # Track unique values (use string representation for comparison)
                    produced_values.add(str(val))
                    uniset.add(val)

                    # Occasionally yield to expose race conditions
                    if i % 100 == 0:
                        time.sleep(0.0001)

            except Exception as e:
                producer_exception = e
            finally:
                # Always close the set when done producing
                uniset.close()

        def consumer():
            """Consumer thread that reads values from the set."""
            nonlocal consumer_exception
            try:
                while True:
                    try:
                        val = uniset.pop()
                        consumed_values.append(str(val))
                    except RuntimeError as e:
                        # Expected when set is closed
                        if "Cannot remove from closed empty set" in str(e):
                            # Reopen the set to drain remaining values
                            uniset.open()
                            remaining = len(uniset)
                            for _ in range(remaining):
                                val = uniset.pop()
                                consumed_values.append(str(val))
                            # Close the set again
                            uniset.close()
                            break
                        else:
                            raise

                    # Occasionally yield to expose race conditions
                    if len(consumed_values) % 100 == 0:
                        time.sleep(0.0001)

            except Exception as e:
                consumer_exception = e

        # Start threads
        producer_thread = threading.Thread(target=producer, name="Producer")
        consumer_thread = threading.Thread(target=consumer, name="Consumer")

        consumer_thread.start()
        producer_thread.start()

        # Wait for both threads to complete
        producer_thread.join(timeout=30)  # 30 second timeout
        consumer_thread.join(timeout=30)

        # Check that threads completed successfully
        self.assertFalse(producer_thread.is_alive(), "Producer thread did not complete")
        self.assertFalse(consumer_thread.is_alive(), "Consumer thread did not complete")

        # Check for exceptions
        if producer_exception:
            raise AssertionError(f"Producer thread failed: {producer_exception}")
        if consumer_exception:
            raise AssertionError(f"Consumer thread failed: {consumer_exception}")

        # For a set, we should have consumed all unique values
        consumed_set = set(consumed_values)

        # Verify all unique values were consumed
        self.assertEqual(len(produced_values), len(consumed_set))

        # Verify the consumed values match produced values
        self.assertEqual(produced_values, consumed_set)

        # Verify set is empty and closed
        self.assertEqual(0, len(uniset))
        self.assertTrue(uniset.is_closed())

    def test_concurrent_multiple_producers_single_consumer(self):
        """Test multiple producers with a single consumer."""
        uniset = UnisetValue()
        uniset.open()

        # Shared state
        producer_exceptions = {}
        consumer_exception = None
        consumed_values = []
        produced_values = set()
        produced_lock = threading.Lock()

        # Configuration
        num_producers = 5
        items_per_producer = 2000

        def producer(producer_id):
            """Producer thread that writes values to the set."""
            try:
                for i in range(items_per_producer):
                    # Create values with producer ID to track origin
                    val = StringValue(f'producer_{producer_id}_item_{i}')
                    uniset.add(val)

                    # Track produced values
                    with produced_lock:
                        produced_values.add(str(val))

                    # Yield occasionally
                    if i % 50 == 0:
                        time.sleep(0.00001)

            except Exception as e:
                producer_exceptions[producer_id] = e

        def consumer():
            """Consumer thread that reads all values."""
            nonlocal consumer_exception
            try:
                while True:
                    try:
                        val = uniset.pop()
                        consumed_values.append(str(val))
                    except RuntimeError as e:
                        # Expected when set is closed
                        if "Cannot remove from closed empty set" in str(e):
                            # Reopen the set to drain remaining values
                            uniset.open()
                            remaining = len(uniset)
                            for _ in range(remaining):
                                val = uniset.pop()
                                consumed_values.append(str(val))
                            # Close the set again
                            uniset.close()
                            break
                        else:
                            raise

            except Exception as e:
                consumer_exception = e

        # Start all threads
        producer_threads = []
        for i in range(num_producers):
            thread = threading.Thread(target=producer, args=(i,), name=f"Producer-{i}")
            producer_threads.append(thread)
            thread.start()

        consumer_thread = threading.Thread(target=consumer, name="Consumer")
        consumer_thread.start()

        # Wait for all producers to complete
        for thread in producer_threads:
            thread.join(timeout=30)
            self.assertFalse(thread.is_alive(), f"{thread.name} did not complete")

        # Close set after all producers are done
        uniset.close()

        # Wait for consumer to complete
        consumer_thread.join(timeout=30)
        self.assertFalse(consumer_thread.is_alive(), "Consumer thread did not complete")

        # Check for exceptions
        for producer_id, exc in producer_exceptions.items():
            raise AssertionError(f"Producer {producer_id} failed: {exc}")
        if consumer_exception:
            raise AssertionError(f"Consumer thread failed: {consumer_exception}")

        # For a set, consumed values should be unique
        consumed_set = set(consumed_values)

        # Verify all unique values were consumed
        self.assertEqual(len(produced_values), len(consumed_set))
        self.assertEqual(produced_values, consumed_set)

    def test_concurrent_add_duplicates(self):
        """Test that concurrent adds handle object identity correctly.

        Note: UnisetValue deduplicates based on object identity, not value equality.
        Adding the same Value object multiple times results in deduplication,
        but different Value objects with the same value are treated as distinct.
        """
        uniset = UnisetValue()
        uniset.open()

        errors = []
        add_count = 0
        lock = threading.Lock()

        # Create shared value objects that threads will reuse
        shared_values = [FloatValue(float(i)) for i in range(10)]

        def adder(thread_id):
            """Add the same value objects repeatedly."""
            nonlocal add_count
            try:
                for i in range(1000):
                    # All threads add the same 10 value objects
                    for val in shared_values:
                        uniset.add(val)
                        with lock:
                            add_count += 1

                    if i % 100 == 0:
                        time.sleep(0.0001)

            except Exception as e:
                errors.append(f"Thread {thread_id} error: {e}")

        # Start multiple threads adding the same value objects
        threads = []
        for i in range(5):
            thread = threading.Thread(target=adder, args=(i,))
            threads.append(thread)
            thread.start()

        # Wait for all threads to complete
        for thread in threads:
            thread.join(timeout=30)

        # Check for errors
        if errors:
            raise AssertionError("Errors occurred: " + "; ".join(errors))

        # Despite many adds, should only have 10 unique value objects
        self.assertEqual(10, len(uniset))

        # Verify the values are correct
        values = []
        for _ in range(10):
            values.append(uniset.pop())

        # Sort by float value for comparison
        values.sort(key=lambda v: v.to_list()[0])

        for i, val in enumerate(values):
            self.assertEqual(float(i), val.to_list()[0])

        uniset.close()

        # Verify total add operations
        self.assertEqual(5 * 1000 * 10, add_count)
        print(f"Added same 10 objects {add_count} times, set size remained 10")

    def test_stress_many_threads(self):
        """Stress test with many threads doing mixed operations."""
        uniset = UnisetValue()
        uniset.open()

        # Configuration
        num_threads = 10
        ops_per_thread = 1000
        thread_errors = {}

        def worker(thread_id):
            """Worker that both adds and removes."""
            try:
                for i in range(ops_per_thread):
                    # Alternate between add and remove
                    if i % 2 == 0:
                        # Add a value specific to this thread and operation
                        uniset.add(StringValue(f'thread_{thread_id}_op_{i}'))
                    else:
                        # Try to remove only if set is not empty to avoid blocking
                        if len(uniset) > 0:
                            try:
                                uniset.pop()
                            except RuntimeError:
                                # Set might be closed, that's OK
                                pass

                    # Tiny random delay
                    time.sleep(random.random() * 0.0001)

            except Exception as e:
                thread_errors[thread_id] = e

        # Start all worker threads
        threads = []
        for i in range(num_threads):
            thread = threading.Thread(target=worker, args=(i,), name=f"Worker-{i}")
            threads.append(thread)
            thread.start()

        # Wait for all threads to complete
        for thread in threads:
            thread.join(timeout=60)
            self.assertFalse(thread.is_alive(), f"{thread.name} did not complete")

        # Check for errors
        for thread_id, exc in thread_errors.items():
            raise AssertionError(f"Thread {thread_id} failed: {exc}")

        # Close set and drain any remaining values
        uniset.close()
        remaining = 0
        uniset.open()  # Reopen to drain
        while len(uniset) > 0:
            uniset.pop()
            remaining += 1
        uniset.close()

        # We added num_threads * (ops_per_thread // 2) values
        # But since it's a set, we have at most that many unique values
        # and we removed some, so remaining should be >= 0
        print(f"Stress test complete. Remaining values in set: {remaining}")
        self.assertGreaterEqual(remaining, 0)
        self.assertLessEqual(remaining, num_threads * (ops_per_thread // 2))


if __name__ == '__main__':
    unittest.main()
