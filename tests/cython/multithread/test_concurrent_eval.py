"""
Phase 1: Baseline Thread Safety Tests

These tests verify basic thread safety of PythonEval and concurrent
function execution through GroundedSchemas.
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

from test_threading_utils import (
    ThreadTestCase, ThreadSafetyValidator, check_memory_leaks
)

# Import helper module for testing
import helper_module


class Test_1_1_ConcurrentEvalCreation(ThreadTestCase):
    """
    Test 1.1: Concurrent PythonEval Creation/Destruction

    Objective: Verify thread-local evaluator instances can be created/destroyed safely
    """

    def setUp(self):
        """Set up test fixtures."""
        pass

    def tearDown(self):
        """Clean up after test."""
        pass

    def test_concurrent_eval_creation_50_threads(self):
        """
        50 threads simultaneously create evaluator instances and execute.

        Each thread:
        1. Creates a new AtomSpace (implicitly creates PythonEval)
        2. Performs simple Python evaluation
        3. Destroys its AtomSpace

        Success: All threads complete without crashes/hangs
        """
        num_threads = 50
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker that creates/destroys evaluator."""
            try:
                # Create new AtomSpace (triggers PythonEval creation)
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Execute a simple GroundedSchema
                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode("py:helper_module.simple_function"),
                    ListLink()
                )

                result = exec_link.execute()

                # Verify result
                if result.name != "success":
                    validator.record_error(
                        thread_id,
                        f"Wrong result: {result.name}"
                    )

                return "success"

            except Exception as e:
                validator.record_error(thread_id, str(e))
                return "failed"

        # Run all threads with barrier synchronization for maximum contention
        results = self.run_threads(
            worker,
            num_threads,
            use_barrier=True,
            timeout=40.0
        )

        # Verify all succeeded
        validator.assert_no_errors(self)
        success_count = sum(1 for r in results.values() if r == "success")
        self.assertEqual(
            success_count, num_threads,
            f"Expected {num_threads} successes, got {success_count}"
        )

    @check_memory_leaks(tolerance_mb=50.0)
    def test_rapid_eval_creation_destruction(self):
        """
        Rapidly execute functions using reused evaluators to detect issues.

        10 threads, each reusing the same AtomSpace for 50 executions.
        Tests for memory leaks and thread-safety issues with realistic usage.
        Note: AtomSpaces are long-lived databases, not meant to be rapidly
        created/destroyed.
        """
        num_threads = 10
        iterations_per_thread = 50
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker that reuses one AtomSpace for multiple executions."""
            success_count = 0

            try:
                # Create ONE long-lived AtomSpace per thread (realistic usage)
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Repeatedly execute using the same atomspace
                for iteration in range(iterations_per_thread):
                    exec_link = ExecutionOutputLink(
                        GroundedSchemaNode("py:helper_module.simple_function"),
                        ListLink()
                    )
                    result = exec_link.execute()

                    if result.name == "success":
                        success_count += 1
                    else:
                        validator.record_error(
                            thread_id,
                            f"Iteration {iteration}: wrong result {result.name}"
                        )
                        break

                # Cleanup at end of thread
                del thread_atomspace

            except Exception as e:
                validator.record_error(thread_id, str(e))

            return success_count

        results = self.run_threads(worker, num_threads, timeout=40.0)

        # Check results
        validator.assert_no_errors(self)
        total_success = sum(results.values())
        expected_total = num_threads * iterations_per_thread
        self.assertEqual(
            total_success, expected_total,
            f"Expected {expected_total} successes, got {total_success}"
        )


class Test_1_2_ConcurrentSameFunction(ThreadTestCase):
    """
    Test 1.2: Concurrent Function Calls (Same Function)

    Objective: Verify multiple threads can call the same Python function simultaneously
    """

    def setUp(self):
        """Set up test fixtures."""
        pass

    def tearDown(self):
        """Clean up after test."""
        pass

    def test_30_threads_same_function(self):
        """
        30 threads call the same function simultaneously.

        Tests for race conditions in function lookup and execution.
        """
        num_threads = 30
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker calling same function."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # All threads call the same function
                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode("py:helper_module.simple_function"),
                    ListLink()
                )

                result = exec_link.execute()

                if result.name != "success":
                    validator.record_error(
                        thread_id,
                        f"Expected 'success', got '{result.name}'"
                    )
                    return "failed"

                return "success"

            except Exception as e:
                validator.record_error(thread_id, str(e))
                return "failed"

        # Use barrier for maximum contention
        results = self.run_threads(
            worker,
            num_threads,
            use_barrier=True,
            timeout=25.0
        )

        validator.assert_no_errors(self)
        success_count = sum(1 for r in results.values() if r == "success")
        self.assertEqual(success_count, num_threads)

    def test_concurrent_with_arguments(self):
        """
        30 threads call same function with different arguments.

        Verifies argument passing is thread-safe.
        """
        num_threads = 30
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker calling function with thread-specific args."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Create thread-specific arguments
                arg1 = ConceptNode(f"arg1_{thread_id}")
                arg2 = ConceptNode(f"arg2_{thread_id}")

                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode("py:helper_module.function_with_args"),
                    ListLink(arg1, arg2)
                )

                result = exec_link.execute()

                # Verify result is correct for this thread
                expected = f"result_arg1_{thread_id}_arg2_{thread_id}"
                if result.name != expected:
                    validator.record_error(
                        thread_id,
                        f"Expected '{expected}', got '{result.name}'"
                    )
                    return "failed"

                return "success"

            except Exception as e:
                validator.record_error(thread_id, str(e))
                return "failed"

        results = self.run_threads(
            worker,
            num_threads,
            use_barrier=True,
            timeout=25.0
        )

        validator.assert_no_errors(self)
        success_count = sum(1 for r in results.values() if r == "success")
        self.assertEqual(success_count, num_threads)



class Test_1_3_ConcurrentDifferentFunctions(ThreadTestCase):
    """
    Test 1.3: Concurrent Function Calls (Different Functions)

    Objective: Verify threads calling different functions don't interfere
    """

    def setUp(self):
        """Set up test fixtures."""
        pass

    def tearDown(self):
        """Clean up after test."""
        pass

    def test_30_threads_different_functions(self):
        """
        30 threads, each calling a different function.

        Verifies function dispatch is thread-safe and correct.
        """
        num_threads = 30
        validator = ThreadSafetyValidator()

        # Define function mapping
        functions = [
            ("helper_module.function_a", "result_a"),
            ("helper_module.function_b", "result_b"),
            ("helper_module.function_c", "result_c"),
            ("helper_module.function_d", "result_d"),
            ("helper_module.function_e", "result_e"),
        ]

        def worker(thread_id):
            """Worker calling function based on thread ID."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Select function based on thread ID
                func_name, expected_result = functions[thread_id % len(functions)]

                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode(f"py:{func_name}"),
                    ListLink()
                )

                result = exec_link.execute()

                if result.name != expected_result:
                    validator.record_error(
                        thread_id,
                        f"Called {func_name}, expected '{expected_result}', "
                        f"got '{result.name}'"
                    )
                    return "failed"

                return expected_result

            except Exception as e:
                validator.record_error(thread_id, str(e))
                return "failed"

        results = self.run_threads(
            worker,
            num_threads,
            use_barrier=True,
            timeout=25.0
        )

        validator.assert_no_errors(self)

        # Verify each thread got its expected result
        for thread_id, result in results.items():
            _, expected = functions[thread_id % len(functions)]
            self.assertEqual(
                result, expected,
                f"Thread {thread_id} got wrong result"
            )

    def test_mixed_signatures(self):
        """
        Threads calling functions with different signatures simultaneously.

        Tests:
        - Functions with no args
        - Functions with 1 arg
        - Functions with 2 args
        """
        num_threads = 30
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker calling function based on thread ID mod 3."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                case = thread_id % 3

                if case == 0:
                    # No arguments
                    exec_link = ExecutionOutputLink(
                        GroundedSchemaNode("py:helper_module.simple_function"),
                        ListLink()
                    )
                    expected = "success"

                elif case == 1:
                    # One argument
                    arg = ConceptNode(f"single_{thread_id}")
                    exec_link = ExecutionOutputLink(
                        GroundedSchemaNode("py:helper_module.function_with_args"),
                        ListLink(arg, arg)
                    )
                    expected = f"result_single_{thread_id}_single_{thread_id}"

                else:  # case == 2
                    # Two arguments
                    arg1 = ConceptNode(f"a_{thread_id}")
                    arg2 = ConceptNode(f"b_{thread_id}")
                    exec_link = ExecutionOutputLink(
                        GroundedSchemaNode("py:helper_module.function_with_args"),
                        ListLink(arg1, arg2)
                    )
                    expected = f"result_a_{thread_id}_b_{thread_id}"

                result = exec_link.execute()

                if result.name != expected:
                    validator.record_error(
                        thread_id,
                        f"Case {case}: expected '{expected}', got '{result.name}'"
                    )
                    return "failed"

                return "success"

            except Exception as e:
                validator.record_error(thread_id, str(e))
                return "failed"

        results = self.run_threads(
            worker,
            num_threads,
            use_barrier=True,
            timeout=25.0
        )

        validator.assert_no_errors(self)
        success_count = sum(1 for r in results.values() if r == "success")
        self.assertEqual(success_count, num_threads)


if __name__ == '__main__':
    unittest.main()
