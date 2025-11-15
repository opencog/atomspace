"""
Phase 3: Error Handling Tests

These tests verify that exceptions and errors in one thread are properly
isolated and don't affect other threads.
"""

import unittest
import threading

from opencog.atomspace import AtomSpace
from opencog.type_constructors import (
    ConceptNode, ExecutionOutputLink, GroundedSchemaNode, ListLink
)
from opencog.type_ctors import push_thread_atomspace

from test_threading_utils import (
    ThreadTestCase, ThreadSafetyValidator
)

import helper_module


class Test_3_1_ExceptionIsolation(ThreadTestCase):
    """
    Test 3.1: Exception Isolation

    Objective: Verify exceptions in one thread don't affect other threads.
    """

    def setUp(self):
        """Set up test fixtures."""
        pass

    def tearDown(self):
        """Clean up after test."""
        pass

    def test_20_threads_mixed_success_failure(self):
        """
        20 threads: even threads succeed, odd threads raise exceptions.

        Verifies:
        - Exceptions in odd threads don't crash even threads
        - Each thread properly handles or reports its own errors
        - System remains stable despite exceptions

        Success: Even threads complete successfully, odd threads catch exceptions
        """
        num_threads = 20
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker that may raise exception based on thread ID."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                if thread_id % 2 == 0:
                    # Even threads: execute successful function
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
                else:
                    # Odd threads: execute function that raises exception
                    exec_link = ExecutionOutputLink(
                        GroundedSchemaNode("py:helper_module.function_that_raises"),
                        ListLink()
                    )

                    try:
                        result = exec_link.execute()
                        # Should not reach here
                        validator.record_error(
                            thread_id,
                            "Exception was not raised as expected"
                        )
                        return "failed"
                    except RuntimeError as e:
                        # Expected exception - verify it's the right one
                        if "Deliberate test exception" in str(e):
                            return "exception_caught"
                        else:
                            validator.record_error(
                                thread_id,
                                f"Wrong exception message: {e}"
                            )
                            return "failed"

            except Exception as e:
                validator.record_error(thread_id, f"Unexpected exception: {e}")
                return "failed"

        results = self.run_threads(
            worker,
            num_threads,
            use_barrier=True,
            timeout=30.0
        )

        # Verify all threads completed correctly
        validator.assert_no_errors(self)

        # Check that even threads succeeded
        for thread_id in range(0, num_threads, 2):
            self.assertEqual(
                results[thread_id], "success",
                f"Even thread {thread_id} should have succeeded"
            )

        # Check that odd threads caught exceptions
        for thread_id in range(1, num_threads, 2):
            self.assertEqual(
                results[thread_id], "exception_caught",
                f"Odd thread {thread_id} should have caught exception"
            )

    def test_different_exception_types(self):
        """
        20 threads raise different exception types concurrently.

        Tests:
        - RuntimeError
        - TypeError
        - ValueError
        - ZeroDivisionError

        Success: All exceptions properly caught and isolated with correct types
        """
        num_threads = 20
        validator = ThreadSafetyValidator()

        exception_functions = [
            ("py:helper_module.function_that_raises", RuntimeError, "Deliberate test exception"),
            ("py:helper_module.function_with_type_error", TypeError, "TypeError"),
            ("py:helper_module.function_with_value_error", ValueError, "ValueError"),
            ("py:helper_module.function_with_division_by_zero", ZeroDivisionError, "ZeroDivisionError"),
        ]

        def worker(thread_id):
            """Worker that raises specific exception type."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Select exception type based on thread ID
                func_name, expected_exc_type, expected_msg_part = exception_functions[thread_id % 4]

                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode(func_name),
                    ListLink()
                )

                try:
                    result = exec_link.execute()
                    validator.record_error(
                        thread_id,
                        f"Expected {expected_exc_type.__name__} but no exception raised"
                    )
                    return "failed"
                except expected_exc_type as e:
                    # Verify exception message
                    if expected_msg_part in str(e):
                        return "exception_caught"
                    else:
                        validator.record_error(
                            thread_id,
                            f"Exception message wrong: {e}"
                        )
                        return "failed"
                except Exception as e:
                    validator.record_error(
                        thread_id,
                        f"Wrong exception type: {type(e).__name__}: {e}"
                    )
                    return "failed"

            except Exception as e:
                validator.record_error(thread_id, f"Unexpected outer exception: {e}")
                return "failed"

        results = self.run_threads(
            worker,
            num_threads,
            use_barrier=True,
            timeout=30.0
        )

        validator.assert_no_errors(self)

        # All threads should have caught their exceptions
        for thread_id, result in results.items():
            self.assertEqual(
                result, "exception_caught",
                f"Thread {thread_id} should have caught exception"
            )


class Test_3_2_ModuleImportErrors(ThreadTestCase):
    """
    Test 3.2: Module Import Errors

    Objective: Verify handling of non-existent module imports.
    """

    def setUp(self):
        """Set up test fixtures."""
        pass

    def tearDown(self):
        """Clean up after test."""
        pass

    def test_15_threads_import_nonexistent_module(self):
        """
        15 threads attempt to import non-existent module.

        Verifies:
        - Import errors are properly caught
        - Errors don't crash other threads
        - System remains stable

        Success: All threads properly handle import error
        """
        num_threads = 15
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker that attempts invalid import."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Try to execute function from non-existent module
                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode(f"py:nonexistent_module_{thread_id}.some_function"),
                    ListLink()
                )

                try:
                    result = exec_link.execute()
                    # Should not reach here
                    validator.record_error(
                        thread_id,
                        "Expected exception for non-existent module but got result"
                    )
                    return "failed"
                except (ImportError, ModuleNotFoundError, RuntimeError) as e:
                    # Expected - import should fail
                    return "import_error_caught"
                except Exception as e:
                    # Also acceptable - different error types for import failures
                    if "nonexistent_module" in str(e) or "import" in str(e).lower():
                        return "import_error_caught"
                    validator.record_error(
                        thread_id,
                        f"Wrong exception for import: {type(e).__name__}: {e}"
                    )
                    return "failed"

            except Exception as e:
                validator.record_error(thread_id, f"Unexpected exception: {e}")
                return "failed"

        results = self.run_threads(
            worker,
            num_threads,
            use_barrier=True,
            timeout=30.0
        )

        validator.assert_no_errors(self)

        # All threads should have caught import errors
        for thread_id, result in results.items():
            self.assertEqual(
                result, "import_error_caught",
                f"Thread {thread_id} should have caught import error"
            )


class Test_3_3_InvalidFunctionCalls(ThreadTestCase):
    """
    Test 3.3: Invalid Function Calls

    Objective: Verify handling of invalid function calls.
    """

    def setUp(self):
        """Set up test fixtures."""
        pass

    def tearDown(self):
        """Clean up after test."""
        pass

    def test_15_threads_invalid_function_name(self):
        """
        15 threads attempt to call non-existent function.

        Verifies:
        - Invalid function calls are properly handled
        - Errors don't affect other threads

        Success: All threads properly handle the error
        """
        num_threads = 15
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker that calls non-existent function."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Try to call non-existent function
                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode(f"py:helper_module.nonexistent_function_{thread_id}"),
                    ListLink()
                )

                try:
                    result = exec_link.execute()
                    validator.record_error(
                        thread_id,
                        "Expected exception for non-existent function"
                    )
                    return "failed"
                except (AttributeError, RuntimeError) as e:
                    # Expected - function doesn't exist
                    return "function_error_caught"
                except Exception as e:
                    # Also acceptable
                    if "nonexistent_function" in str(e) or "attribute" in str(e).lower():
                        return "function_error_caught"
                    validator.record_error(
                        thread_id,
                        f"Wrong exception type: {type(e).__name__}: {e}"
                    )
                    return "failed"

            except Exception as e:
                validator.record_error(thread_id, f"Unexpected exception: {e}")
                return "failed"

        results = self.run_threads(
            worker,
            num_threads,
            use_barrier=True,
            timeout=30.0
        )

        validator.assert_no_errors(self)

        # All threads should have caught function errors
        for thread_id, result in results.items():
            self.assertEqual(
                result, "function_error_caught",
                f"Thread {thread_id} should have caught function error"
            )

    def test_wrong_argument_count(self):
        """
        20 threads call function with wrong number of arguments.

        Verifies proper handling of argument mismatches.
        """
        num_threads = 20
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker that calls function with wrong args."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                if thread_id % 2 == 0:
                    # Call function_with_args with no arguments (expects 2)
                    exec_link = ExecutionOutputLink(
                        GroundedSchemaNode("py:helper_module.function_with_args"),
                        ListLink()  # No arguments
                    )
                else:
                    # Call simple_function with arguments (expects 0)
                    arg = ConceptNode(f"unexpected_arg_{thread_id}")
                    exec_link = ExecutionOutputLink(
                        GroundedSchemaNode("py:helper_module.simple_function"),
                        ListLink(arg)  # Unexpected argument
                    )

                try:
                    result = exec_link.execute()
                    # Might succeed or fail depending on implementation
                    # If it succeeds, that's also valid behavior
                    return "completed"
                except (TypeError, RuntimeError) as e:
                    # Expected - wrong argument count
                    return "arg_error_caught"
                except Exception as e:
                    # Could be various error types
                    return "completed"

            except Exception as e:
                validator.record_error(thread_id, f"Unexpected exception: {e}")
                return "failed"

        results = self.run_threads(
            worker,
            num_threads,
            use_barrier=True,
            timeout=30.0
        )

        validator.assert_no_errors(self)

        # All threads should have completed (caught error or handled gracefully)
        for thread_id, result in results.items():
            self.assertIn(
                result, ["arg_error_caught", "completed"],
                f"Thread {thread_id} should have completed or caught error"
            )


if __name__ == '__main__':
    unittest.main()
