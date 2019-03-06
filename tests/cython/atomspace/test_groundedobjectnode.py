import unittest

from opencog.atomspace import AtomSpace
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import (GroundedObjectNode,
                                       ApplyLink, MethodOfLink,
                                       ListLink, ConceptNode)
from opencog.bindlink import execute_atom

class GroundedObjectNodeTest(unittest.TestCase):

    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_call_grounded_object_predicate(self):
        grounded_object_node = GroundedObjectNode("test_grounded_object_node",
                                                  TestObject("some object"))
        exec_link = ApplyLink(
                        MethodOfLink(grounded_object_node, ConceptNode("foo")),
                        ListLink(ConceptNode("arg"))
                        )

        result = execute_atom(self.space,  exec_link)

        self.assertEqual(result, ConceptNode("arg"))

    def test_call_grounded_object_no_arguments(self):
        grounded_object_node = GroundedObjectNode("test_grounded_object_node",
                                                  TestObject("some object"))
        exec_link = ApplyLink(
                        MethodOfLink(grounded_object_node,
                                     ConceptNode("noarguments")),
                        ListLink()
                    )

        result = execute_atom(self.space,  exec_link)

        self.assertEqual(result, ConceptNode("empty"))

    def test_call_grounded_object_predicate_two_args(self):
        grounded_object_node = GroundedObjectNode("test_grounded_object_node",
                                                  TestObject("some object"))
        exec_link = ApplyLink(
                        MethodOfLink(grounded_object_node, ConceptNode("second")),
                        ListLink(
                            ConceptNode("firstArg"),
                            ConceptNode("secondArg")
                        )
                    )

        result = execute_atom(self.space,  exec_link)

        self.assertEqual(result, ConceptNode("secondArg"))

    def test_set_object(self):
        grounded_object_node = GroundedObjectNode("test_grounded_object_node",
                                                  TestObject("some object"))
        grounded_object_node.set_object(TestObject("other object"))

        self.assertEqual(grounded_object_node.get_object().name, "other object")

    def test_create_grounded_object_node_without_object(self):
        grounded_object_node = GroundedObjectNode("test_grounded_object_node")

        self.assertTrue(grounded_object_node.get_object() is None)

class TestObject:

    def __init__(self, name):
        self.name = name

    def foo(self, arg):
        return arg

    def noarguments(self):
        return ConceptNode("empty")

    def second(self, first, second):
        return second

if __name__ == '__main__':
    unittest.main()
