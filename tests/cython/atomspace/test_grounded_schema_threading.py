"""
Unit test to reproduce Python module resolution bug in multi-threaded AtomSpace contexts.

This test creates multiple threads, where each thread:
1. Creates a new AtomSpace
2. Executes a GroundedSchema that calls a Python function from an imported module
3. Reports success or failure

The test is designed to expose race conditions in module resolution when
using GroundedSchema with threading.
"""

import unittest
import threading
import time

from opencog.atomspace import AtomSpace
from opencog.type_constructors import (
    ConceptNode, ExecutionOutputLink, GroundedSchemaNode,
    ListLink, NumberNode
)
from opencog.type_ctors import push_thread_atomspace

# Import the test helper module
import test_helper_module

# Also register a function in __main__ for control test
import __main__


def main_module_function():
    """Function registered in __main__ for control testing."""
    return ConceptNode("main_success")


def main_function_with_args(arg1, arg2):
    """Function with args registered in __main__."""
    return ConceptNode(f"main_result_{arg1.name}_{arg2.name}")


__main__.main_module_function = main_module_function
__main__.main_function_with_args = main_function_with_args


class GroundedSchemaThreadingTest(unittest.TestCase):
    """Test GroundedSchema execution in multi-threaded contexts."""

    def setUp(self):
        """Set up test fixtures."""
        pass

    def tearDown(self):
        """Clean up after test."""
        pass

    def test_grounded_schema_multithread_external_module(self):
        """
        Test GroundedSchema with external module in multiple threads.

        This test creates multiple threads where each thread:
        - Creates a new AtomSpace
        - Executes a GroundedSchema calling test_helper_module.simple_function
        - Records success or any errors

        Expected: All threads should succeed.
        Bug: Some threads may fail with "module not found" error.
        """
        num_threads = 15
        thread_results = {}
        thread_exceptions = {}
        barrier = threading.Barrier(num_threads)

        def thread_worker(thread_id):
            """Worker function for each thread."""
            try:
                # Synchronize thread start to maximize race condition exposure
                barrier.wait()

                # Create a new AtomSpace for this thread
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Create and execute GroundedSchema with external module
                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode("py:test_helper_module.simple_function"),
                    ListLink()
                )

                result = exec_link.execute()

                # Verify result
                if result.name == "success":
                    thread_results[thread_id] = "success"
                else:
                    thread_results[thread_id] = f"unexpected_result:{result.name}"

            except Exception as e:
                thread_exceptions[thread_id] = str(e)
                thread_results[thread_id] = "failed"

        # Create and start all threads
        threads = []
        for i in range(num_threads):
            thread = threading.Thread(
                target=thread_worker,
                args=(i,),
                name=f"Worker-{i}"
            )
            threads.append(thread)
            thread.start()

        # Wait for all threads to complete
        for thread in threads:
            thread.join(timeout=30)
            self.assertFalse(
                thread.is_alive(),
                f"{thread.name} did not complete in time"
            )

        # Analyze results
        success_count = sum(1 for r in thread_results.values() if r == "success")
        failed_count = len(thread_exceptions)

        # Report detailed results
        if failed_count > 0:
            error_summary = "\n".join(
                f"  Thread {tid}: {exc}"
                for tid, exc in thread_exceptions.items()
            )
            self.fail(
                f"GroundedSchema execution failed in {failed_count}/{num_threads} threads.\n"
                f"This indicates a module resolution bug in multi-threaded contexts.\n"
                f"Errors:\n{error_summary}"
            )

        # All threads should succeed
        self.assertEqual(
            success_count, num_threads,
            f"Expected all {num_threads} threads to succeed, "
            f"but only {success_count} succeeded"
        )

    def test_grounded_schema_multithread_with_arguments(self):
        """
        Test GroundedSchema with arguments in multiple threads.

        This test verifies that module resolution works correctly when
        passing arguments to the grounded schema function.
        """
        num_threads = 15
        thread_results = {}
        thread_exceptions = {}
        barrier = threading.Barrier(num_threads)

        def thread_worker(thread_id):
            """Worker function for each thread."""
            try:
                # Synchronize thread start
                barrier.wait()

                # Create a new AtomSpace for this thread
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Create test atoms
                arg1 = ConceptNode(f"arg1_{thread_id}")
                arg2 = ConceptNode(f"arg2_{thread_id}")

                # Execute GroundedSchema with arguments
                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode("py:test_helper_module.function_with_args"),
                    ListLink(arg1, arg2)
                )

                result = exec_link.execute()

                # Verify result contains expected pattern
                expected_name = f"result_arg1_{thread_id}_arg2_{thread_id}"
                if result.name == expected_name:
                    thread_results[thread_id] = "success"
                else:
                    thread_results[thread_id] = f"unexpected:{result.name}"

            except Exception as e:
                thread_exceptions[thread_id] = str(e)
                thread_results[thread_id] = "failed"

        # Create and start all threads
        threads = []
        for i in range(num_threads):
            thread = threading.Thread(
                target=thread_worker,
                args=(i,),
                name=f"ArgWorker-{i}"
            )
            threads.append(thread)
            thread.start()

        # Wait for all threads to complete
        for thread in threads:
            thread.join(timeout=30)
            self.assertFalse(
                thread.is_alive(),
                f"{thread.name} did not complete in time"
            )

        # Check for failures
        if thread_exceptions:
            error_summary = "\n".join(
                f"  Thread {tid}: {exc}"
                for tid, exc in thread_exceptions.items()
            )
            self.fail(
                f"GroundedSchema with arguments failed in "
                f"{len(thread_exceptions)}/{num_threads} threads.\n"
                f"Errors:\n{error_summary}"
            )

        # Verify all threads succeeded
        success_count = sum(1 for r in thread_results.values() if r == "success")
        self.assertEqual(
            success_count, num_threads,
            f"Expected {num_threads} successes, got {success_count}"
        )

    def test_grounded_schema_multithread_main_module(self):
        """
        Control test using __main__ module approach.

        This test uses the __main__ module registration pattern (similar to
        test_callback_arity.py) to verify if the issue is specific to
        external module imports or affects all module resolution.
        """
        num_threads = 15
        thread_results = {}
        thread_exceptions = {}
        barrier = threading.Barrier(num_threads)

        def thread_worker(thread_id):
            """Worker function for each thread."""
            try:
                # Synchronize thread start
                barrier.wait()

                # Create a new AtomSpace for this thread
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Execute GroundedSchema with __main__ module function
                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode("py:main_module_function"),
                    ListLink()
                )

                result = exec_link.execute()

                # Verify result
                if result.name == "main_success":
                    thread_results[thread_id] = "success"
                else:
                    thread_results[thread_id] = f"unexpected:{result.name}"

            except Exception as e:
                thread_exceptions[thread_id] = str(e)
                thread_results[thread_id] = "failed"

        # Create and start all threads
        threads = []
        for i in range(num_threads):
            thread = threading.Thread(
                target=thread_worker,
                args=(i,),
                name=f"MainWorker-{i}"
            )
            threads.append(thread)
            thread.start()

        # Wait for all threads to complete
        for thread in threads:
            thread.join(timeout=30)
            self.assertFalse(
                thread.is_alive(),
                f"{thread.name} did not complete in time"
            )

        # Check results
        if thread_exceptions:
            error_summary = "\n".join(
                f"  Thread {tid}: {exc}"
                for tid, exc in thread_exceptions.items()
            )
            self.fail(
                f"GroundedSchema with __main__ module failed in "
                f"{len(thread_exceptions)}/{num_threads} threads.\n"
                f"Errors:\n{error_summary}"
            )

        success_count = sum(1 for r in thread_results.values() if r == "success")
        self.assertEqual(
            success_count, num_threads,
            f"Expected {num_threads} successes, got {success_count}"
        )

    def test_grounded_schema_stress_test(self):
        """
        Stress test with many threads and iterations.

        This test creates a high-load scenario with many threads repeatedly
        executing GroundedSchema to maximize the chance of reproducing the bug.
        """
        num_threads = 20
        iterations_per_thread = 5
        thread_exceptions = {}
        total_executions = {}

        def thread_worker(thread_id):
            """Worker that executes multiple times."""
            try:
                successes = 0
                for iteration in range(iterations_per_thread):
                    # Create new AtomSpace for each iteration
                    thread_atomspace = AtomSpace()
                    push_thread_atomspace(thread_atomspace)

                    # Execute GroundedSchema
                    exec_link = ExecutionOutputLink(
                        GroundedSchemaNode("py:test_helper_module.simple_function"),
                        ListLink()
                    )

                    result = exec_link.execute()

                    if result.name == "success":
                        successes += 1

                    # Small delay to allow thread interleaving
                    time.sleep(0.001)

                total_executions[thread_id] = successes

            except Exception as e:
                thread_exceptions[thread_id] = str(e)
                total_executions[thread_id] = 0

        # Create and start all threads
        threads = []
        for i in range(num_threads):
            thread = threading.Thread(
                target=thread_worker,
                args=(i,),
                name=f"StressWorker-{i}"
            )
            threads.append(thread)
            thread.start()

        # Wait for all threads to complete
        for thread in threads:
            thread.join(timeout=60)
            self.assertFalse(
                thread.is_alive(),
                f"{thread.name} did not complete in time"
            )

        # Analyze results
        expected_total = num_threads * iterations_per_thread
        actual_total = sum(total_executions.values())

        if thread_exceptions:
            error_summary = "\n".join(
                f"  Thread {tid}: {exc}"
                for tid, exc in thread_exceptions.items()
            )
            self.fail(
                f"Stress test failed with {len(thread_exceptions)} thread errors.\n"
                f"Successfully executed: {actual_total}/{expected_total}\n"
                f"Errors:\n{error_summary}"
            )

        self.assertEqual(
            actual_total, expected_total,
            f"Expected {expected_total} successful executions, got {actual_total}"
        )


if __name__ == '__main__':
    unittest.main()
