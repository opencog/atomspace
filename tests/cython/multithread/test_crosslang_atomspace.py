"""
Cross-Language AtomSpace Tests (Test 1a, 1b, 1c)

These tests characterize the current behavior of cross-language atomspace
management between Python and Scheme. They verify thread safety and atomspace
context behavior when:

Test 1a: Scheme code calls Python functions via GroundedSchema "py:..."
Test 1b: Python code calls Scheme functions via GroundedSchema "scm:..."
Test 1c: Cross-language calls when set_thread_atomspace/cog-set-atomspace! is
         used during the call
"""

import unittest
import threading

from opencog.atomspace import AtomSpace
from opencog.type_constructors import (
    ConceptNode, PredicateNode, ListLink, NumberNode,
    ExecutionOutputLink, GroundedSchemaNode, Evaluation,
    GroundedPredicate
)
from opencog.type_ctors import (
    set_thread_atomspace, push_thread_atomspace, get_thread_atomspace
)
from opencog.scheme import scheme_eval, scheme_eval_v

from test_threading_utils import ThreadTestCase, ThreadSafetyValidator

import __main__


# ============================================================================
# Python functions to be called from Scheme (for Test 1a)
# ============================================================================

def crosslang_py_simple():
    """Simple Python function to be called from Scheme."""
    return ConceptNode("py_result")


def crosslang_py_with_args(arg1, arg2):
    """Python function with arguments, called from Scheme."""
    return ConceptNode(f"py_result_{arg1.name}_{arg2.name}")


def crosslang_py_get_atomspace_name():
    """Return the name of the current Python thread atomspace."""
    asp = get_thread_atomspace()
    if asp is None:
        return ConceptNode("no_atomspace")
    # Get atoms to identify which atomspace we're in
    atoms = asp.get_atoms_by_type(0)  # types.Atom = 0
    return ConceptNode(f"py_as_size_{len(atoms)}")


def crosslang_py_create_marker(marker_name):
    """Create a marker atom in the current Python atomspace."""
    return ConceptNode(f"marker_{marker_name.name}")


# Register Python functions in __main__ for GroundedSchema access
__main__.crosslang_py_simple = crosslang_py_simple
__main__.crosslang_py_with_args = crosslang_py_with_args
__main__.crosslang_py_get_atomspace_name = crosslang_py_get_atomspace_name
__main__.crosslang_py_create_marker = crosslang_py_create_marker


class Test_1a_SchemeToPython(ThreadTestCase):
    """
    Test 1a: Multi-threaded Scheme→Python Execution

    Objective: Verify that multiple threads can safely execute Scheme code
    that calls Python functions via GroundedSchema "py:..."
    """

    def test_basic_scheme_to_python(self):
        """
        Basic test: Single thread executes Scheme that calls Python.

        Verifies the basic mechanism works before testing multi-threaded.
        """
        atomspace = AtomSpace()
        push_thread_atomspace(atomspace)

        # Load required Scheme modules
        scheme_eval(atomspace, '(use-modules (opencog exec))')

        # Execute Scheme code that calls Python
        result = scheme_eval_v(atomspace, '''
            (cog-execute!
                (ExecutionOutput
                    (GroundedSchema "py:crosslang_py_simple")
                    (List)))
        ''')

        self.assertIsNotNone(result)
        self.assertEqual(result.name, "py_result")

    def test_scheme_to_python_with_args(self):
        """
        Single thread: Scheme calls Python with arguments.
        """
        atomspace = AtomSpace()
        push_thread_atomspace(atomspace)

        # Load required Scheme modules
        scheme_eval(atomspace, '(use-modules (opencog exec))')

        result = scheme_eval_v(atomspace, '''
            (cog-execute!
                (ExecutionOutput
                    (GroundedSchema "py:crosslang_py_with_args")
                    (List (Concept "foo") (Concept "bar"))))
        ''')

        self.assertIsNotNone(result)
        self.assertEqual(result.name, "py_result_foo_bar")

    def test_15_threads_scheme_to_python(self):
        """
        15 threads simultaneously execute Scheme code calling Python.

        Each thread:
        1. Creates its own AtomSpace
        2. Executes Scheme code that calls a Python function
        3. Verifies the result
        """
        num_threads = 15
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker executing Scheme→Python."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Load required Scheme modules
                scheme_eval(thread_atomspace, '(use-modules (opencog exec))')

                # Execute Scheme that calls Python with thread-specific args
                result = scheme_eval_v(thread_atomspace, f'''
                    (cog-execute!
                        (ExecutionOutput
                            (GroundedSchema "py:crosslang_py_with_args")
                            (List (Concept "thread_{thread_id}_a")
                                  (Concept "thread_{thread_id}_b"))))
                ''')

                expected = f"py_result_thread_{thread_id}_a_thread_{thread_id}_b"
                if result is None:
                    validator.record_error(thread_id, "Result is None")
                    return "failed"
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
            timeout=30.0
        )

        validator.assert_no_errors(self)
        success_count = sum(1 for r in results.values() if r == "success")
        self.assertEqual(success_count, num_threads)

    def test_30_threads_scheme_to_python_intensive(self):
        """
        30 threads with intensive Scheme→Python execution.

        Each thread executes multiple Scheme→Python calls.
        """
        num_threads = 30
        calls_per_thread = 5
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker with multiple Scheme→Python calls."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Load required Scheme modules
                scheme_eval(thread_atomspace, '(use-modules (opencog exec))')

                for call_idx in range(calls_per_thread):
                    result = scheme_eval_v(thread_atomspace, f'''
                        (cog-execute!
                            (ExecutionOutput
                                (GroundedSchema "py:crosslang_py_simple")
                                (List)))
                    ''')

                    if result is None or result.name != "py_result":
                        validator.record_error(
                            thread_id,
                            f"Call {call_idx}: unexpected result"
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
            timeout=60.0
        )

        validator.assert_no_errors(self)
        success_count = sum(1 for r in results.values() if r == "success")
        self.assertEqual(success_count, num_threads)


class Test_1b_PythonToScheme(ThreadTestCase):
    """
    Test 1b: Multi-threaded Python→Scheme Execution

    Objective: Verify that multiple threads can safely execute Python code
    that calls Scheme functions via GroundedSchema "scm:..."
    """

    def test_basic_python_to_scheme(self):
        """
        Basic test: Single thread executes Python that calls Scheme.

        Verifies the basic mechanism works before testing multi-threaded.
        """
        atomspace = AtomSpace()
        push_thread_atomspace(atomspace)

        # First define a Scheme function
        scheme_eval(atomspace, '''
            (define (scm-simple-func)
                (Concept "scm_result"))
        ''')

        # Now call it via GroundedSchema
        exec_link = ExecutionOutputLink(
            GroundedSchemaNode("scm: scm-simple-func"),
            ListLink()
        )

        result = exec_link.execute()

        self.assertIsNotNone(result)
        self.assertEqual(result.name, "scm_result")

    def test_python_to_scheme_with_args(self):
        """
        Single thread: Python calls Scheme with arguments.
        """
        atomspace = AtomSpace()
        push_thread_atomspace(atomspace)

        # Define Scheme function that takes arguments
        scheme_eval(atomspace, '''
            (define (scm-with-args arg1 arg2)
                (Concept (string-append "scm_result_"
                    (cog-name arg1) "_" (cog-name arg2))))
        ''')

        arg1 = ConceptNode("alpha")
        arg2 = ConceptNode("beta")

        exec_link = ExecutionOutputLink(
            GroundedSchemaNode("scm: scm-with-args"),
            ListLink(arg1, arg2)
        )

        result = exec_link.execute()

        self.assertIsNotNone(result)
        self.assertEqual(result.name, "scm_result_alpha_beta")

    def test_15_threads_python_to_scheme(self):
        """
        15 threads simultaneously execute Python code calling Scheme.

        Each thread:
        1. Creates its own AtomSpace
        2. Defines a Scheme function in that AtomSpace
        3. Executes Python code that calls the Scheme function
        4. Verifies the result
        """
        num_threads = 15
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker executing Python→Scheme."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Define Scheme function in this atomspace
                scheme_eval(thread_atomspace, f'''
                    (define (thread-{thread_id}-func arg1 arg2)
                        (Concept (string-append "scm_t{thread_id}_"
                            (cog-name arg1) "_" (cog-name arg2))))
                ''')

                # Call Scheme from Python with thread-specific args
                arg1 = ConceptNode(f"a{thread_id}")
                arg2 = ConceptNode(f"b{thread_id}")

                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode(f"scm: thread-{thread_id}-func"),
                    ListLink(arg1, arg2)
                )

                result = exec_link.execute()

                expected = f"scm_t{thread_id}_a{thread_id}_b{thread_id}"
                if result is None:
                    validator.record_error(thread_id, "Result is None")
                    return "failed"
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
            timeout=30.0
        )

        validator.assert_no_errors(self)
        success_count = sum(1 for r in results.values() if r == "success")
        self.assertEqual(success_count, num_threads)

    def test_15_threads_shared_scheme_function(self):
        """
        15 threads call the same Scheme function defined in a shared atomspace.

        This tests that Scheme function lookup is thread-safe when the
        function is defined in a parent/shared atomspace.
        """
        num_threads = 15
        validator = ThreadSafetyValidator()

        # Create shared atomspace with the Scheme function
        shared_atomspace = AtomSpace()
        push_thread_atomspace(shared_atomspace)

        scheme_eval(shared_atomspace, '''
            (define (shared-scm-func arg)
                (Concept (string-append "shared_" (cog-name arg))))
        ''')

        def worker(thread_id):
            """Worker calling shared Scheme function."""
            try:
                # Use the shared atomspace
                push_thread_atomspace(shared_atomspace)

                arg = ConceptNode(f"t{thread_id}")

                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode("scm: shared-scm-func"),
                    ListLink(arg)
                )

                result = exec_link.execute()

                expected = f"shared_t{thread_id}"
                if result is None:
                    validator.record_error(thread_id, "Result is None")
                    return "failed"
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
            timeout=30.0
        )

        validator.assert_no_errors(self)
        success_count = sum(1 for r in results.values() if r == "success")
        self.assertEqual(success_count, num_threads)


class Test_1c_CrossLangWithAtomspaceSwitch(ThreadTestCase):
    """
    Test 1c: Cross-language calls with atomspace switching

    Objective: Test behavior when set_thread_atomspace/cog-set-atomspace! is
    used during a cross-language call. This characterizes the current behavior
    which may need to change in the future.
    """

    def test_scheme_to_python_atomspace_visible(self):
        """
        Test: When Scheme calls Python, which atomspace does Python see?

        This characterizes current behavior for documentation purposes.
        """
        atomspace = AtomSpace()
        push_thread_atomspace(atomspace)

        # Load required Scheme modules
        scheme_eval(atomspace, '(use-modules (opencog exec))')

        # Add a marker to identify this atomspace
        ConceptNode("original_marker")

        # Execute Scheme that calls Python to check atomspace
        result = scheme_eval_v(atomspace, '''
            (cog-execute!
                (ExecutionOutput
                    (GroundedSchema "py:crosslang_py_get_atomspace_name")
                    (List)))
        ''')

        self.assertIsNotNone(result)
        # The result tells us what Python sees
        print(f"Scheme→Python: Python sees atomspace: {result.name}")

    def test_python_to_scheme_atomspace_visible(self):
        """
        Test: When Python calls Scheme, which atomspace does Scheme see?

        This characterizes current behavior for documentation purposes.
        """
        atomspace = AtomSpace()
        push_thread_atomspace(atomspace)

        # Add markers
        ConceptNode("py_marker")

        # Define Scheme function that checks atomspace
        scheme_eval(atomspace, '''
            (define (scm-check-atomspace)
                (Concept (format #f "scm_as_size_~a"
                    (length (cog-get-atoms 'Atom)))))
        ''')

        exec_link = ExecutionOutputLink(
            GroundedSchemaNode("scm: scm-check-atomspace"),
            ListLink()
        )

        result = exec_link.execute()

        self.assertIsNotNone(result)
        print(f"Python→Scheme: Scheme sees atomspace: {result.name}")


if __name__ == '__main__':
    unittest.main()
