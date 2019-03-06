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

    def test_call_grounded_object_call(self):
        exec_link = ApplyLink(
                        MethodOfLink(
                            GroundedObjectNode("obj", TestObject("obj")),
                            ConceptNode("get_argument")
                        ),
                        ListLink(
                            ConceptNode("arg")
                        )
                    )

        result = execute_atom(self.space,  exec_link)

        self.assertEqual(result, ConceptNode("arg"))

    def test_call_grounded_object_no_arguments(self):
        exec_link = ApplyLink(
                        MethodOfLink(
                            GroundedObjectNode("obj", TestObject("obj")),
                            ConceptNode("no_arguments")
                        ),
                        ListLink()
                    )

        result = execute_atom(self.space,  exec_link)

        self.assertEqual(result, ConceptNode("empty"))

    def test_call_grounded_object_predicate_two_args(self):
        exec_link = ApplyLink(
                        MethodOfLink(
                            GroundedObjectNode("obj", TestObject("obj")),
                            ConceptNode("get_second")
                        ),
                        ListLink(
                            ConceptNode("first"),
                            ConceptNode("second")
                        )
                    )

        result = execute_atom(self.space,  exec_link)

        self.assertEqual(result, ConceptNode("second"))

    def test_set_object(self):
        grounded_object_node = GroundedObjectNode("a", TestObject("some object"))
        grounded_object_node.set_object(TestObject("other object"))

        self.assertEqual(grounded_object_node.get_object().name, "other object")

    def test_create_grounded_object_node_without_object(self):
        grounded_object_node = GroundedObjectNode("obj")

        self.assertTrue(grounded_object_node.get_object() is None)

class TestObject:

    def __init__(self, name):
        self.name = name

    def get_argument(self, arg):
        return arg

    def no_arguments(self):
        return ConceptNode("empty")

    def get_second(self, first, second):
        return second

if __name__ == '__main__':
    unittest.main()
