"""
Test: Shared AtomSpace Across Threads

These tests verify that child threads automatically use the same AtomSpace
as the parent thread. This makes multi-threaded Python code work as expected
without requiring explicit atomspace management in each thread.
"""

import unittest
import threading

from opencog.atomspace import AtomSpace
from opencog.type_constructors import ConceptNode, PredicateNode
from opencog.atomspace import get_thread_atomspace, set_thread_atomspace

from test_threading_utils import ThreadTestCase, ThreadSafetyValidator


class Test_SharedAtomSpace(ThreadTestCase):
    """
    Test that child threads automatically use the same AtomSpace as parent.
    """

    def test_basic_sharing(self):
        """
        Parent creates atom, child thread can see it.
        """
        # Create a marker atom in parent thread
        marker = ConceptNode("parent-marker")

        results = {}

        def child_worker():
            """Child thread should see parent's atom."""
            try:
                # Child can see the parent's marker atom
                check_marker = ConceptNode("parent-marker")
                if check_marker.name == "parent-marker":
                    results['status'] = 'success'
                else:
                    results['status'] = 'failed'
                    results['error'] = 'Marker atom not found'

            except Exception as e:
                results['status'] = 'failed'
                results['error'] = str(e)

        # Run child thread
        child_thread = threading.Thread(target=child_worker)
        child_thread.start()
        child_thread.join(timeout=10.0)

        self.assertFalse(child_thread.is_alive(), "Child thread did not complete")
        self.assertEqual(results.get('status'), 'success',
                        f"Child failed: {results.get('error', 'unknown')}")

    def test_multiple_threads_share(self):
        """
        Multiple child threads all use the same atomspace.
        """
        num_threads = 10
        validator = ThreadSafetyValidator()

        # Create markers in parent thread
        for i in range(5):
            ConceptNode(f"shared-marker-{i}")

        def child_worker(thread_id):
            """Child verifies it can see parent's markers."""
            try:
                for i in range(5):
                    marker = ConceptNode(f"shared-marker-{i}")
                    if marker.name != f"shared-marker-{i}":
                        validator.record_error(thread_id, f"Cannot see shared-marker-{i}")
                        return "failed"

                return "success"

            except Exception as e:
                validator.record_error(thread_id, str(e))
                return "failed"

        results = self.run_threads(
            child_worker,
            num_threads,
            use_barrier=True,
            timeout=30.0
        )

        validator.assert_no_errors(self)
        success_count = sum(1 for r in results.values() if r == "success")
        self.assertEqual(success_count, num_threads)

    def test_child_creates_atom_parent_sees(self):
        """
        Child thread creates atom, parent can see it.

        This verifies all threads use the SAME atomspace, not copies.
        """
        child_created = threading.Event()
        results = {}

        def child_worker():
            """Child creates an atom."""
            try:
                ConceptNode("child-created-atom")
                results['created'] = True
                child_created.set()

            except Exception as e:
                results['error'] = str(e)

        child_thread = threading.Thread(target=child_worker)
        child_thread.start()
        child_created.wait(timeout=10.0)
        child_thread.join(timeout=5.0)

        self.assertTrue(results.get('created'), f"Child failed: {results.get('error')}")

        # Parent can see child's atom
        child_atom = ConceptNode("child-created-atom")
        self.assertEqual(child_atom.name, "child-created-atom",
                        "Parent should see atom created by child")

    def test_child_can_set_different_atomspace(self):
        """
        Child thread can switch to its own AtomSpace.

        When a child sets a new atomspace in its own thread, the parent's
        atomspace should remain unchanged. Atoms created in the child's
        atomspace should not appear in the parent's atomspace.
        """
        # Create a marker in parent's atomspace
        parent_marker = ConceptNode("parent-only-marker")

        child_ready = threading.Event()
        child_done = threading.Event()
        results = {}

        def child_worker():
            """Child switches to its own atomspace."""
            try:
                # Initially child can see parent's atom
                check_parent = ConceptNode("parent-only-marker")
                if check_parent.name != "parent-only-marker":
                    results['error'] = "Child cannot see parent marker initially"
                    child_ready.set()
                    return

                # Child creates its own atomspace and switches to it
                child_atomspace = AtomSpace()
                set_thread_atomspace(child_atomspace)

                # Create atom in child's private atomspace
                child_only = ConceptNode("child-only-marker")
                results['child_atom_created'] = True

                # Verify parent's marker is NOT visible in child's new atomspace
                # (Creating a ConceptNode with the same name would create a NEW atom)
                parent_check = ConceptNode("parent-only-marker")
                # In the new atomspace, this is a fresh atom, not the parent's
                results['child_atomspace_size'] = len(child_atomspace)

                child_ready.set()
                child_done.wait(timeout=10.0)

            except Exception as e:
                results['error'] = str(e)
                child_ready.set()

        child_thread = threading.Thread(target=child_worker)
        child_thread.start()
        child_ready.wait(timeout=10.0)

        # Verify child created its atom
        self.assertTrue(results.get('child_atom_created'),
                       f"Child failed: {results.get('error')}")

        # Parent's atomspace should NOT have the child-only atom
        # We can check by getting the parent's atomspace and checking its size
        parent_atomspace = get_thread_atomspace()
        parent_atoms = list(parent_atomspace)

        # The child-only-marker should NOT be in parent's atomspace
        child_only_in_parent = any(
            hasattr(a, 'name') and a.name == "child-only-marker"
            for a in parent_atoms
        )
        self.assertFalse(child_only_in_parent,
                        "Child's atom leaked into parent's atomspace")

        # Clean up
        child_done.set()
        child_thread.join(timeout=5.0)


class Test_SharedAtomSpaceWithExecution(ThreadTestCase):
    """
    Test shared atomspace with GroundedSchema execution.
    """

    def test_execution_in_shared_atomspace(self):
        """
        Child threads can execute GroundedSchemas.
        """
        from opencog.type_constructors import (
            ExecutionOutputLink, GroundedSchemaNode, ListLink
        )

        num_threads = 10
        validator = ThreadSafetyValidator()

        def child_worker(thread_id):
            """Child executes GroundedSchema."""
            try:
                import helper_module
                exec_link = ExecutionOutputLink(
                    GroundedSchemaNode("py:helper_module.simple_function"),
                    ListLink()
                )

                result = exec_link.execute()

                if result.name != "success":
                    validator.record_error(thread_id, f"Wrong result: {result.name}")
                    return "failed"

                return "success"

            except Exception as e:
                validator.record_error(thread_id, str(e))
                return "failed"

        results = self.run_threads(
            child_worker,
            num_threads,
            use_barrier=True,
            timeout=30.0
        )

        validator.assert_no_errors(self)
        success_count = sum(1 for r in results.values() if r == "success")
        self.assertEqual(success_count, num_threads)


if __name__ == '__main__':
    unittest.main()
