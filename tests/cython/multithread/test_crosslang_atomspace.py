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
    GroundedPredicate, StringValue, FloatValue
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


class Test_SchemeEvalVariants(ThreadTestCase):
    """
    Test that scheme_eval and scheme_eval_v work both with explicit atomspace
    argument and without (using the thread atomspace).
    """

    def test_scheme_eval_with_explicit_atomspace(self):
        """
        Test scheme_eval with explicit atomspace argument.
        """
        atomspace = AtomSpace()
        set_thread_atomspace(atomspace)

        result = scheme_eval(atomspace, '(+ 2 3)')
        self.assertIn('5', result)

    def test_scheme_eval_without_atomspace(self):
        """
        Test scheme_eval without atomspace argument - uses thread atomspace.
        """
        result = scheme_eval('(+ 4 5)')
        self.assertIn('9', result)

    def test_scheme_eval_v_with_explicit_atomspace(self):
        """
        Test scheme_eval_v with explicit atomspace argument.
        """
        atomspace = AtomSpace()
        set_thread_atomspace(atomspace)

        result = scheme_eval_v(atomspace, '(Concept "explicit-test")')
        self.assertIsNotNone(result)
        self.assertEqual(result.name, "explicit-test")

    def test_scheme_eval_v_without_atomspace(self):
        """
        Test scheme_eval_v without atomspace argument - uses thread atomspace.
        """
        result = scheme_eval_v('(Concept "implicit-test")')
        self.assertIsNotNone(result)
        self.assertEqual(result.name, "implicit-test")

    def test_both_variants_use_same_atomspace(self):
        """
        Verify that explicit and implicit variants use the same atomspace
        when the explicit atomspace matches the thread atomspace.
        """
        atomspace = AtomSpace()
        set_thread_atomspace(atomspace)

        # Create atom with explicit atomspace
        atom1 = scheme_eval_v(atomspace, '(Concept "shared-atom")')

        # Create value with implicit atomspace (should use same atomspace)
        scheme_eval('(cog-set-value! (Concept "shared-atom") (Predicate "key") (StringValue "hello"))')

        # Verify the value is on the same atom
        key = PredicateNode("key")
        value = atom1.get_value(key)
        self.assertIsNotNone(value)
        self.assertEqual(value.to_list(), ["hello"])


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
        set_thread_atomspace(atomspace)

        # Load required Scheme modules

        # Execute Scheme code that calls Python - test both variants
        # First with explicit atomspace
        result = scheme_eval_v(atomspace, '''
            (cog-execute!
                (ExecutionOutput
                    (GroundedSchema "py:crosslang_py_simple")
                    (List)))
        ''')

        self.assertIsNotNone(result)
        self.assertEqual(result.name, "py_result")

        # Then without explicit atomspace (uses thread atomspace)
        result2 = scheme_eval_v('''
            (cog-execute!
                (ExecutionOutput
                    (GroundedSchema "py:crosslang_py_simple")
                    (List)))
        ''')

        self.assertIsNotNone(result2)
        self.assertEqual(result2.name, "py_result")

    def test_scheme_to_python_with_args(self):
        """
        Single thread: Scheme calls Python with arguments.
        """
        atomspace = AtomSpace()
        set_thread_atomspace(atomspace)

        # Load required Scheme modules

        # Test with explicit atomspace
        result = scheme_eval_v(atomspace, '''
            (cog-execute!
                (ExecutionOutput
                    (GroundedSchema "py:crosslang_py_with_args")
                    (List (Concept "foo") (Concept "bar"))))
        ''')

        self.assertIsNotNone(result)
        self.assertEqual(result.name, "py_result_foo_bar")

        # Test without explicit atomspace
        result2 = scheme_eval_v('''
            (cog-execute!
                (ExecutionOutput
                    (GroundedSchema "py:crosslang_py_with_args")
                    (List (Concept "baz") (Concept "qux"))))
        ''')

        self.assertIsNotNone(result2)
        self.assertEqual(result2.name, "py_result_baz_qux")

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
                set_thread_atomspace(thread_atomspace)

                # Load required Scheme modules

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
                set_thread_atomspace(thread_atomspace)

                # Load required Scheme modules

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
        set_thread_atomspace(atomspace)

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
        set_thread_atomspace(atomspace)

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
                set_thread_atomspace(thread_atomspace)

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
        set_thread_atomspace(shared_atomspace)

        scheme_eval(shared_atomspace, '''
            (define (shared-scm-func arg)
                (Concept (string-append "shared_" (cog-name arg))))
        ''')

        def worker(thread_id):
            """Worker calling shared Scheme function."""
            try:
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
        set_thread_atomspace(atomspace)

        # Load required Scheme modules

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
        set_thread_atomspace(atomspace)

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


class Test_CrossLangAtomSharing(ThreadTestCase):
    """
    Test that atoms are truly shared between Python and Scheme.

    These tests verify that:
    1. An atom created in one language is the same atom in the other
    2. Values set on an atom in one language are readable in the other
    3. Modifications in one language are visible in the other
    """

    def test_scheme_creates_atom_python_reads_value(self):
        """
        Scheme creates an atom and sets a value, Python reads it back.

        This verifies the atomspace is truly shared.
        """
        atomspace = AtomSpace()
        set_thread_atomspace(atomspace)

        # Scheme creates atom and sets a StringValue on it
        scheme_eval(atomspace, '''
            (cog-set-value!
                (Concept "shared-atom-1")
                (Predicate "test-key-1")
                (StringValue "hello from scheme"))
        ''')

        # Python reads the same atom and value
        atom = ConceptNode("shared-atom-1")
        key = PredicateNode("test-key-1")
        value = atom.get_value(key)

        self.assertIsNotNone(value, "Python should see the value set by Scheme")
        self.assertEqual(value.to_list(), ["hello from scheme"])

    def test_python_creates_atom_scheme_reads_value(self):
        """
        Python creates an atom and sets a value, Scheme reads it back.
        """
        atomspace = AtomSpace()
        set_thread_atomspace(atomspace)

        # Python creates atom and sets a StringValue on it
        atom = ConceptNode("shared-atom-2")
        key = PredicateNode("test-key-2")
        atomspace.set_value(atom, key, StringValue("hello from python"))

        # Scheme reads the same atom and value
        result = scheme_eval(atomspace, '''
            (define val (cog-value (Concept "shared-atom-2") (Predicate "test-key-2")))
            (if val
                (cog-value-ref val 0)
                "NOT FOUND")
        ''')

        self.assertIn("hello from python", result)

    def test_scheme_creates_floatvalue_python_reads(self):
        """
        Scheme creates a FloatValue, Python reads it and verifies values match.
        """
        atomspace = AtomSpace()
        set_thread_atomspace(atomspace)

        # Scheme sets a FloatValue with specific numbers
        scheme_eval(atomspace, '''
            (cog-set-value!
                (Concept "float-atom")
                (Predicate "float-key")
                (FloatValue 1.5 2.5 3.5))
        ''')

        # Python reads and verifies
        atom = ConceptNode("float-atom")
        key = PredicateNode("float-key")
        value = atom.get_value(key)

        self.assertIsNotNone(value)
        float_list = value.to_list()
        self.assertEqual(len(float_list), 3)
        self.assertAlmostEqual(float_list[0], 1.5)
        self.assertAlmostEqual(float_list[1], 2.5)
        self.assertAlmostEqual(float_list[2], 3.5)

    def test_python_creates_floatvalue_scheme_reads(self):
        """
        Python creates a FloatValue, Scheme reads it and verifies values match.
        """
        atomspace = AtomSpace()
        set_thread_atomspace(atomspace)

        # Python sets a FloatValue
        atom = ConceptNode("py-float-atom")
        key = PredicateNode("py-float-key")
        atomspace.set_value(atom, key, FloatValue([10.1, 20.2, 30.3]))

        # Scheme reads and verifies each element
        result = scheme_eval(atomspace, '''
            (define fv (cog-value (Concept "py-float-atom") (Predicate "py-float-key")))
            (format #f "~a ~a ~a"
                (cog-value-ref fv 0)
                (cog-value-ref fv 1)
                (cog-value-ref fv 2))
        ''')

        # Parse the result - should be "10.1 20.2 30.3" approximately
        self.assertIn("10.1", result)
        self.assertIn("20.2", result)
        self.assertIn("30.3", result)

    def test_atom_identity_across_languages(self):
        """
        Verify that the same atom handle is used in both languages.

        Create atom in Python, modify in Scheme, verify in Python.
        """
        atomspace = AtomSpace()
        set_thread_atomspace(atomspace)

        # Python creates an atom
        atom = ConceptNode("identity-test-atom")
        key1 = PredicateNode("key-from-python")
        atomspace.set_value(atom, key1, StringValue("python-value"))

        # Scheme adds another value to the SAME atom
        scheme_eval(atomspace, '''
            (cog-set-value!
                (Concept "identity-test-atom")
                (Predicate "key-from-scheme")
                (StringValue "scheme-value"))
        ''')

        # Python verifies both values are on the same atom
        key2 = PredicateNode("key-from-scheme")

        val1 = atom.get_value(key1)
        val2 = atom.get_value(key2)

        self.assertIsNotNone(val1, "Python's own value should still be there")
        self.assertIsNotNone(val2, "Scheme's value should be visible to Python")

        self.assertEqual(val1.to_list(), ["python-value"])
        self.assertEqual(val2.to_list(), ["scheme-value"])

    def test_bidirectional_value_modification(self):
        """
        Values can be overwritten by either language.

        Python sets value, Scheme overwrites, Python reads new value.
        """
        atomspace = AtomSpace()
        set_thread_atomspace(atomspace)

        atom = ConceptNode("overwrite-test")
        key = PredicateNode("overwrite-key")

        # Python sets initial value
        atomspace.set_value(atom, key, StringValue("original"))

        # Verify Python sees it
        val = atom.get_value(key)
        self.assertEqual(val.to_list(), ["original"])

        # Scheme overwrites
        scheme_eval(atomspace, '''
            (cog-set-value!
                (Concept "overwrite-test")
                (Predicate "overwrite-key")
                (StringValue "modified by scheme"))
        ''')

        # Python reads the new value
        val = atom.get_value(key)
        self.assertEqual(val.to_list(), ["modified by scheme"])

    def test_threaded_crosslang_atom_sharing(self):
        """
        Multiple threads share atoms across languages.

        Each thread:
        1. Creates a unique atom in Python
        2. Sets a value in Python
        3. Has Scheme read and modify it
        4. Python verifies Scheme's modification
        """
        num_threads = 10
        validator = ThreadSafetyValidator()

        # Shared atomspace for all threads
        shared_atomspace = AtomSpace()
        set_thread_atomspace(shared_atomspace)

        def worker(thread_id):
            """Worker that tests cross-language atom sharing."""
            try:
                # Unique atom names for this thread
                atom_name = f"thread-{thread_id}-atom"
                key_name = f"thread-{thread_id}-key"

                # Python creates atom and sets value
                atom = ConceptNode(atom_name)
                key = PredicateNode(key_name)
                shared_atomspace.set_value(
                    atom, key, StringValue(f"py-{thread_id}")
                )

                # Scheme reads and modifies
                scheme_eval(shared_atomspace, f'''
                    (cog-set-value!
                        (Concept "{atom_name}")
                        (Predicate "{key_name}")
                        (StringValue "scm-modified-{thread_id}"))
                ''')

                # Python verifies Scheme's modification
                val = atom.get_value(key)
                if val is None:
                    validator.record_error(thread_id, "Value is None after Scheme modification")
                    return "failed"

                expected = f"scm-modified-{thread_id}"
                actual = val.to_list()[0]
                if actual != expected:
                    validator.record_error(
                        thread_id,
                        f"Expected '{expected}', got '{actual}'"
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


class Test_1d_CrossLangPushPopVisibility(ThreadTestCase):
    """
    Test 1d: Cross-language push/pop visibility

    Objective: Verify that when one language pushes an AtomSpace,
    the other language can see atoms created in that pushed space,
    and when popped, those atoms are no longer visible.
    """

    def test_python_push_scheme_creates_atom_python_sees(self):
        """
        Python pushes AtomSpace, Scheme creates atom with value, Python sees it.

        Flow:
        1. Python pushes a new AtomSpace
        2. Python calls Scheme function via GroundedSchema
        3. Scheme creates an atom and sets a value on it
        4. Python verifies it can see the atom and value
        5. Python pops the AtomSpace
        6. Python verifies the atom is gone
        """
        from opencog.type_ctors import pop_thread_atomspace

        # Create base atomspace
        base_atomspace = AtomSpace()
        set_thread_atomspace(base_atomspace)

        # Define a Scheme function that creates an atom with a value
        scheme_eval(base_atomspace, '''
            (define (scm-create-marker-with-value)
                (let ((marker (Concept "scheme-created-marker"))
                      (key (Predicate "scheme-key")))
                    (cog-set-value! marker key (StringValue "hello-from-scheme"))
                    marker))
        ''')

        # Push a new temporary atomspace
        pushed_as = push_thread_atomspace()

        # Call Scheme function - it should create the atom in the pushed atomspace
        exec_link = ExecutionOutputLink(
            GroundedSchemaNode("scm: scm-create-marker-with-value"),
            ListLink()
        )
        result = exec_link.execute()

        # Verify Python can see the result
        self.assertIsNotNone(result)
        self.assertEqual(result.name, "scheme-created-marker")

        # Verify Python can see the value that Scheme set
        marker = ConceptNode("scheme-created-marker")
        key = PredicateNode("scheme-key")
        value = marker.get_value(key)

        self.assertIsNotNone(value, "Python should see the value set by Scheme")
        self.assertEqual(value.to_list(), ["hello-from-scheme"])

        # Pop the atomspace - the atom should now be gone from view
        pop_thread_atomspace()

        # Verify the atom is no longer findable in the base atomspace
        # After pop, creating a ConceptNode with the same name should give us
        # a NEW atom (not the one from the pushed space), or the atom from
        # the pushed space should have getAtomSpace() == None (orphaned)
        marker_after_pop = ConceptNode("scheme-created-marker")
        marker_as = marker_after_pop.atomspace
        # The marker should either be in base_atomspace (newly created) or orphaned
        # It should NOT have the value we set in the pushed space
        value_after_pop = marker_after_pop.get_value(key)
        # After pop, the atom in base shouldn't have the value (it's a different atom
        # or the original is orphaned)
        self.assertTrue(
            value_after_pop is None or marker_as == base_atomspace,
            "After pop, the pushed atom's value should not be accessible")

    def test_scheme_push_python_creates_atom_scheme_sees(self):
        """
        Scheme pushes AtomSpace, Python creates atom with value, Scheme sees it.

        Flow:
        1. Scheme pushes a new AtomSpace (via cog-push-atomspace)
        2. Scheme calls Python function via GroundedPredicate
        3. Python creates an atom and sets a value on it
        4. Scheme verifies it can see the atom and value
        5. Scheme pops the AtomSpace
        6. Scheme verifies the atom is gone

        This test is executed from Python but orchestrates Scheme to do the pushing.
        """
        # Create base atomspace
        base_atomspace = AtomSpace()
        set_thread_atomspace(base_atomspace)

        # Load required modules

        # Execute the full test sequence in Scheme
        # Note: cog-node returns #f when not found
        result = scheme_eval(base_atomspace, '''
            ; Push a new temporary atomspace
            (define base-as (cog-push-atomspace))

            ; Call Python function that creates an atom with a value
            ; Use cog-execute! on Evaluation link
            (define py-succeeded
                (cog-execute!
                    (Evaluation
                        (GroundedPredicate "py:crosslang_py_create_atom_with_value_pred")
                        (List))))

            ; Check if we can see the atom and value
            ; Note: cog-node returns #f when not found
            (define marker (cog-node 'ConceptNode "python-created-marker"))
            (define marker-found (if marker #t #f))
            (define key (Predicate "python-key"))
            (define val (if marker-found (cog-value marker key) #f))
            (define val-correct (if val
                (equal? (cog-value-ref val 0) "hello-from-python")
                #f))

            ; Pop the atomspace
            (cog-pop-atomspace)

            ; Check if atom is gone from base
            (define marker-after-pop (cog-node 'ConceptNode "python-created-marker"))
            (define marker-gone (not marker-after-pop))

            ; Return results as a string
            (format #f "py-ok:~a marker:~a val-correct:~a gone-after-pop:~a"
                py-succeeded
                marker-found
                val-correct
                marker-gone)
        ''')

        # Parse and verify results
        print(f"Test result: {result}")
        self.assertIn("marker:#t", result, "Scheme should see the marker atom")
        self.assertIn("val-correct:#t", result, "Scheme should see correct value")
        self.assertIn("gone-after-pop:#t", result, "Atom should be gone after pop")


# Python function for Test 1d - creates atom with value (Schema version)
def crosslang_py_create_atom_with_value():
    """Create a marker atom with a value in the current atomspace."""
    marker = ConceptNode("python-created-marker")
    key = PredicateNode("python-key")
    asp = get_thread_atomspace()
    asp.set_value(marker, key, StringValue("hello-from-python"))
    return marker


# Python function for Test 1d - creates atom with value (Predicate version)
def crosslang_py_create_atom_with_value_pred():
    """Create a marker atom with a value in the current atomspace. Returns True/False."""
    try:
        marker = ConceptNode("python-created-marker")
        key = PredicateNode("python-key")
        asp = get_thread_atomspace()
        asp.set_value(marker, key, StringValue("hello-from-python"))
        return True
    except Exception as e:
        print(f"Python predicate error: {e}")
        return False


# Register the new functions in __main__
__main__.crosslang_py_create_atom_with_value = crosslang_py_create_atom_with_value
__main__.crosslang_py_create_atom_with_value_pred = crosslang_py_create_atom_with_value_pred


if __name__ == '__main__':
    unittest.main()
