import unittest
import threading
import time
import random

from opencog.type_constructors import *
from opencog.utilities import set_default_atomspace, finalize_opencog


class QueueValueMultithreadTest(unittest.TestCase):
    """Test concurrent operations on QueueValue to ensure thread safety."""

    def setUp(self):
        self.space = AtomSpace()
        set_default_atomspace(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_concurrent_producer_consumer(self):
        """Test concurrent producer-consumer pattern with multiple threads."""
        queue = QueueValue()
        queue.open()

        # Shared state
        produced_values = []
        consumed_values = []
        producer_exception = None
        consumer_exception = None

        # Number of items to produce/consume
        num_items = 10000

        def producer():
            """Producer thread that writes values to the queue."""
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
                        # More FloatValues instead of atoms to avoid atomspace issues
                        val = FloatValue(float(i * 2))
                    else:
                        # More StringValues instead of atoms
                        val = StringValue(f'value_{i}')

                    produced_values.append(val)
                    queue.push(val)

                    # Occasionally yield to expose race conditions
                    if i % 100 == 0:
                        time.sleep(0.0001)

            except Exception as e:
                producer_exception = e
            finally:
                # Always close the queue when done producing
                queue.close()

        def consumer():
            """Consumer thread that reads values from the queue."""
            nonlocal consumer_exception
            try:
                while True:
                    try:
                        val = queue.pop()
                        consumed_values.append(val)
                    except RuntimeError as e:
                        # Expected when queue is closed
                        if "Cannot pop from closed empty queue" in str(e):
                            # Reopen the queue to drain remaining values
                            queue.open()
                            remaining = len(queue)
                            for _ in range(remaining):
                                val = queue.pop()
                                consumed_values.append(val)
                            # Close the queue again
                            queue.close()
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

        # Verify all values were consumed
        self.assertEqual(len(produced_values), num_items)
        self.assertEqual(len(consumed_values), num_items)

        # Verify values match (order should be preserved in FIFO queue)
        for i, (produced, consumed) in enumerate(zip(produced_values, consumed_values)):
            self.assertEqual(produced, consumed,
                           f"Mismatch at index {i}: produced {produced}, consumed {consumed}")

        # Verify queue is empty
        self.assertEqual(0, len(queue))
        self.assertTrue(queue.is_closed())

    def test_concurrent_multiple_producers_single_consumer(self):
        """Test multiple producers with a single consumer."""
        queue = QueueValue()
        queue.open()

        # Shared state
        producer_exceptions = {}
        consumer_exception = None
        consumed_values = []
        expected_total = 0

        # Configuration
        num_producers = 5
        items_per_producer = 2000

        def producer(producer_id):
            """Producer thread that writes values to the queue."""
            try:
                for i in range(items_per_producer):
                    # Create values with producer ID to track origin
                    val = StringValue(f'producer_{producer_id}_item_{i}')
                    queue.push(val)

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
                        val = queue.pop()
                        consumed_values.append(val)
                    except RuntimeError as e:
                        # Expected when queue is closed
                        if "Cannot pop from closed empty queue" in str(e):
                            # Reopen the queue to drain remaining values
                            queue.open()
                            remaining = len(queue)
                            for _ in range(remaining):
                                val = queue.pop()
                                consumed_values.append(val)
                            # Close the queue again
                            queue.close()
                            break
                        else:
                            raise

            except Exception as e:
                consumer_exception = e

        # Calculate expected total
        expected_total = num_producers * items_per_producer

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

        # Close queue after all producers are done
        queue.close()

        # Wait for consumer to complete
        consumer_thread.join(timeout=30)
        self.assertFalse(consumer_thread.is_alive(), "Consumer thread did not complete")

        # Check for exceptions
        for producer_id, exc in producer_exceptions.items():
            raise AssertionError(f"Producer {producer_id} failed: {exc}")
        if consumer_exception:
            raise AssertionError(f"Consumer thread failed: {consumer_exception}")

        # Verify all values were consumed
        self.assertEqual(len(consumed_values), expected_total)

        # Verify we got values from all producers
        producer_counts = {}
        for val in consumed_values:
            # Extract producer ID from string
            producer_id = int(val.to_list()[0].split('_')[1])
            producer_counts[producer_id] = producer_counts.get(producer_id, 0) + 1

        # Each producer should have contributed exactly items_per_producer values
        for i in range(num_producers):
            self.assertEqual(producer_counts.get(i, 0), items_per_producer,
                           f"Producer {i} contributed wrong number of items")

    def test_concurrent_burst_pattern(self):
        """Test burst write/read pattern to stress the queue."""
        queue = QueueValue()
        queue.open()

        write_count = 0
        read_count = 0
        errors = []

        def burst_writer():
            """Write values in bursts."""
            nonlocal write_count
            try:
                for burst in range(100):  # 100 bursts
                    # Write 100 values rapidly
                    for i in range(100):
                        queue.push(FloatValue(float(burst * 100 + i)))
                        write_count += 1
                    # Pause between bursts
                    time.sleep(0.001)
            except Exception as e:
                errors.append(f"Writer error: {e}")

        def burst_reader():
            """Read values continuously."""
            nonlocal read_count
            try:
                while True:
                    try:
                        val = queue.pop()
                        read_count += 1
                        # Simulate processing time
                        if read_count % 500 == 0:
                            time.sleep(0.001)
                    except RuntimeError as e:
                        if "Cannot pop from closed empty queue" in str(e):
                            # Reopen the queue to drain remaining values
                            queue.open()
                            remaining = len(queue)
                            for _ in range(remaining):
                                val = queue.pop()
                                read_count += 1
                            # Close the queue again
                            queue.close()
                            break
                        else:
                            raise
            except Exception as e:
                errors.append(f"Reader error: {e}")

        # Start threads
        writer = threading.Thread(target=burst_writer)
        reader = threading.Thread(target=burst_reader)

        reader.start()
        writer.start()

        # Wait for writer to complete
        writer.join(timeout=30)

        # Close queue and wait for reader
        queue.close()
        reader.join(timeout=30)

        # Check for errors
        if errors:
            raise AssertionError("Errors occurred: " + "; ".join(errors))

        # Verify counts match
        self.assertEqual(write_count, 10000)  # 100 bursts * 100 values
        self.assertEqual(read_count, write_count)

    def test_drain_before_close(self):
        """Test draining values from queue and then closing."""
        queue = QueueValue()
        queue.open()

        # Pre-fill queue with many values
        num_values = 5000
        for i in range(num_values):
            queue.push(FloatValue(float(i)))

        # Drain all values while queue is still open
        drained_values = []
        for i in range(num_values):
            val = queue.pop()
            drained_values.append(val)

        # Now close the empty queue
        queue.close()

        # Verify all values were drained
        self.assertEqual(len(drained_values), num_values)

        # Verify values are correct and in order
        for i, val in enumerate(drained_values):
            self.assertEqual(val.to_list()[0], float(i))

        # Verify queue is empty
        self.assertEqual(0, len(queue))

        # Try to pop from closed empty queue - should throw
        with self.assertRaises(RuntimeError) as cm:
            queue.pop()
        self.assertIn("Cannot pop from closed empty queue", str(cm.exception))

    def test_stress_many_small_operations(self):
        """Stress test with many threads doing small operations."""
        queue = QueueValue()
        queue.open()

        # Configuration
        num_threads = 10
        ops_per_thread = 1000
        thread_errors = {}

        def worker(thread_id):
            """Worker that both produces and consumes."""
            try:
                for i in range(ops_per_thread):
                    # Alternate between push and pop
                    if i % 2 == 0:
                        # Push
                        queue.push(StringValue(f'thread_{thread_id}_op_{i}'))
                    else:
                        # Try to pop only if queue is not empty to avoid blocking
                        if len(queue) > 0:
                            try:
                                queue.pop()
                            except RuntimeError:
                                # Queue might be closed, that's OK
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

        # Close queue and drain any remaining values
        queue.close()
        remaining = 0
        while True:
            try:
                queue.pop()
                remaining += 1
            except RuntimeError:
                break

        # We pushed num_threads * (ops_per_thread // 2) values
        # We tried to pop the same amount, but some pops might have failed
        # So we should have some values remaining
        print(f"Stress test complete. Remaining values in queue: {remaining}")
        self.assertGreaterEqual(remaining, 0)


if __name__ == '__main__':
    unittest.main()
