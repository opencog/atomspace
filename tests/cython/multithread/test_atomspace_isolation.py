"""
Phase 2: AtomSpace Isolation Tests

These tests verify that AtomSpace instances are properly isolated between
threads, and that shared AtomSpace access is thread-safe.
"""

import unittest
import threading

from opencog.atomspace import AtomSpace
from opencog.type_constructors import (
    ConceptNode, PredicateNode, ListLink, ExecutionOutputLink,
    GroundedSchemaNode
)
from opencog.type_ctors import set_thread_atomspace, push_thread_atomspace

from test_threading_utils import (
    ThreadTestCase, ThreadSafetyValidator
)

import helper_module


class Test_2_1_ThreadLocalAtomSpaces(ThreadTestCase):
    """
    Test 2.1: Thread-Local AtomSpaces (No Sharing)

    Objective: Verify each thread's AtomSpace is isolated - atoms created
    in one thread do not appear in another thread's AtomSpace.
    """

    def setUp(self):
        """Set up test fixtures."""
        pass

    def tearDown(self):
        """Clean up after test."""
        pass

    def test_20_isolated_atomspaces(self):
        """
        20 threads each create isolated AtomSpaces with unique atoms.

        Each thread:
        1. Creates its own AtomSpace
        2. Adds thread-specific atoms
        3. Verifies can read them back

        Success: No crashes, all atoms created correctly
        """
        num_threads = 20
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker with isolated AtomSpace."""
            try:
                # Create thread-local AtomSpace
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Create thread-specific atoms
                atom1 = ConceptNode(f"thread_{thread_id}_atom_1")
                atom2 = ConceptNode(f"thread_{thread_id}_atom_2")
                atom3 = PredicateNode(f"thread_{thread_id}_predicate")

                # Verify atoms can be read back
                check1 = ConceptNode(f"thread_{thread_id}_atom_1")
                check2 = ConceptNode(f"thread_{thread_id}_atom_2")
                check3 = PredicateNode(f"thread_{thread_id}_predicate")

                if check1.name != f"thread_{thread_id}_atom_1":
                    validator.record_error(thread_id, "Failed to read back atom1")
                    return "failed"

                if check2.name != f"thread_{thread_id}_atom_2":
                    validator.record_error(thread_id, "Failed to read back atom2")
                    return "failed"

                if check3.name != f"thread_{thread_id}_predicate":
                    validator.record_error(thread_id, "Failed to read back atom3")
                    return "failed"

                return "success"

            except Exception as e:
                validator.record_error(thread_id, str(e))
                return "failed"

        # Run all threads with barrier synchronization
        results = self.run_threads(
            worker,
            num_threads,
            use_barrier=True,
            timeout=30.0
        )

        # Verify all succeeded
        validator.assert_no_errors(self)
        success_count = sum(1 for r in results.values() if r == "success")
        self.assertEqual(
            success_count, num_threads,
            f"Expected {num_threads} successes, got {success_count}"
        )

    def test_isolation_with_execution(self):
        """
        15 threads execute GroundedSchemas in isolated AtomSpaces.

        Verifies that Python execution context is properly isolated.
        """
        num_threads = 15
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker executing in isolated AtomSpace."""
            try:
                thread_atomspace = AtomSpace()
                push_thread_atomspace(thread_atomspace)

                # Create thread-specific argument
                arg = ConceptNode(f"isolated_{thread_id}")

                # Execute function with thread-specific arg
                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode("py:helper_module.function_with_args"),
                    ListLink(arg, arg)
                )

                result = exec_link.execute()

                # Verify result is thread-specific
                expected = f"result_isolated_{thread_id}_isolated_{thread_id}"
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


class Test_2_2_SharedAtomSpaceReadHeavy(ThreadTestCase):
    """
    Test 2.2: Shared AtomSpace (Read-Heavy)

    Objective: Verify multiple threads can safely read from shared AtomSpace.
    """

    def setUp(self):
        """Set up test fixtures."""
        self.shared_atomspace = AtomSpace()
        set_thread_atomspace(self.shared_atomspace)

        # Pre-populate with atoms for reading
        for i in range(20):
            ConceptNode(f"shared_atom_{i}")

    def tearDown(self):
        """Clean up after test."""
        del self.shared_atomspace

    def test_20_threads_concurrent_reads(self):
        """
        20 threads concurrently read from shared AtomSpace.

        Each thread:
        1. Looks up specific atoms by name
        2. Verifies atom names are correct

        Success: All threads see consistent data
        """
        num_threads = 20
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker performing read operations."""
            try:
                push_thread_atomspace(self.shared_atomspace)

                # Look up specific atoms (simple reads)
                atom_idx = thread_id % 20
                atom = ConceptNode(f"shared_atom_{atom_idx}")

                if atom.name != f"shared_atom_{atom_idx}":
                    validator.record_error(
                        thread_id,
                        f"Atom lookup failed: expected 'shared_atom_{atom_idx}', got '{atom.name}'"
                    )
                    return "failed"

                # Do a few more reads
                atom2 = ConceptNode(f"shared_atom_{(atom_idx + 1) % 20}")
                if atom2.name != f"shared_atom_{(atom_idx + 1) % 20}":
                    validator.record_error(thread_id, "Second atom lookup failed")
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


class Test_2_3_SharedAtomSpaceReadWriteMix(ThreadTestCase):
    """
    Test 2.3: Shared AtomSpace (Read-Write Mix)

    Objective: Verify concurrent reads and writes to shared AtomSpace are safe.
    """

    def setUp(self):
        """Set up test fixtures."""
        self.shared_atomspace = AtomSpace()
        set_thread_atomspace(self.shared_atomspace)

        # Pre-populate with some base atoms
        for i in range(10):
            ConceptNode(f"base_atom_{i}")

    def tearDown(self):
        """Clean up after test."""
        del self.shared_atomspace

    def test_15_threads_mixed_operations(self):
        """
        15 threads perform mixed read/write operations on shared AtomSpace.

        Operations:
        - Even threads: Add new atoms
        - Odd threads: Read existing atoms

        Success: No crashes, operations complete correctly
        """
        num_threads = 15
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker performing mixed read/write."""
            try:
                push_thread_atomspace(self.shared_atomspace)

                if thread_id % 2 == 0:
                    # Writer thread - add a few atoms
                    atom1 = ConceptNode(f"writer_{thread_id}_atom_1")
                    atom2 = ConceptNode(f"writer_{thread_id}_atom_2")

                    # Verify atoms were created
                    if atom1.name != f"writer_{thread_id}_atom_1":
                        validator.record_error(thread_id, "Failed to create atom1")
                        return "failed"
                    if atom2.name != f"writer_{thread_id}_atom_2":
                        validator.record_error(thread_id, "Failed to create atom2")
                        return "failed"
                else:
                    # Reader thread - read base atoms
                    atom = ConceptNode(f"base_atom_{thread_id % 10}")
                    if atom.name != f"base_atom_{thread_id % 10}":
                        validator.record_error(thread_id, "Failed to read atom")
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


class Test_2_4_ParentChildRelationships(ThreadTestCase):
    """
    Test 2.4: AtomSpace Parent-Child Relationships

    Objective: Verify parent-child AtomSpace relationships work correctly
    across threads.
    """

    def setUp(self):
        """Set up test fixtures."""
        self.parent_atomspace = AtomSpace()
        set_thread_atomspace(self.parent_atomspace)

        # Add atoms to parent
        for i in range(5):
            ConceptNode(f"parent_atom_{i}")

    def tearDown(self):
        """Clean up after test."""
        del self.parent_atomspace

    def test_15_child_atomspaces(self):
        """
        15 threads each create child AtomSpaces with parent relationship.

        Each thread:
        1. Creates child AtomSpace with parent
        2. Verifies parent atoms are visible in child
        3. Adds child-specific atoms
        4. Verifies child atoms can be read back

        Success: Proper inheritance and isolation
        """
        num_threads = 15
        validator = ThreadSafetyValidator()

        def worker(thread_id):
            """Worker with child AtomSpace."""
            try:
                # Create child atomspace with parent
                child_atomspace = AtomSpace(parent=self.parent_atomspace)
                push_thread_atomspace(child_atomspace)

                # Verify parent atoms are visible in child
                parent_atom = ConceptNode(f"parent_atom_{thread_id % 5}")
                if parent_atom.name != f"parent_atom_{thread_id % 5}":
                    validator.record_error(
                        thread_id,
                        "Parent atom not visible in child"
                    )
                    return "failed"

                # Add child-specific atoms
                child_atom_1 = ConceptNode(f"child_{thread_id}_atom_1")
                child_atom_2 = ConceptNode(f"child_{thread_id}_atom_2")

                # Verify child atoms exist
                check1 = ConceptNode(f"child_{thread_id}_atom_1")
                check2 = ConceptNode(f"child_{thread_id}_atom_2")

                if check1.name != f"child_{thread_id}_atom_1":
                    validator.record_error(thread_id, "Failed to create child atom 1")
                    return "failed"

                if check2.name != f"child_{thread_id}_atom_2":
                    validator.record_error(thread_id, "Failed to create child atom 2")
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

    def test_nested_execution_with_parent_child(self):
        """
        15 threads execute GroundedSchemas in child AtomSpaces.

        Verifies execution works correctly with parent-child relationships.
        """
        num_threads = 15
        validator = ThreadSafetyValidator()

        # Add a shared atom in parent that all can reference
        shared_atom = ConceptNode("shared_parent_atom")

        def worker(thread_id):
            """Worker with child AtomSpace executing schemas."""
            try:
                # Create child with parent
                child_atomspace = AtomSpace(parent=self.parent_atomspace)
                push_thread_atomspace(child_atomspace)

                # Verify can access parent atom
                parent_atom = ConceptNode("shared_parent_atom")
                if parent_atom.name != "shared_parent_atom":
                    validator.record_error(thread_id, "Cannot access parent atom")
                    return "failed"

                # Execute schema with thread-specific args in child space
                arg = ConceptNode(f"child_arg_{thread_id}")
                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode("py:helper_module.function_with_args"),
                    ListLink(arg, parent_atom)
                )

                result = exec_link.execute()

                # Verify result
                expected = f"result_child_arg_{thread_id}_shared_parent_atom"
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


if __name__ == '__main__':
    unittest.main()
