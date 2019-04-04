import unittest

from opencog.atomspace import AtomSpace
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *
from opencog.bindlink import execute_atom
import __main__
import sys

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

    def test_call_grounded_object_without_arguments_wrapping(self):
        exec_link = ApplyLink(
                        MethodOfLink(
                            GroundedObjectNode("obj", TestObject("obj"),
                                               unwrap_args = True),
                            ConceptNode("plain_multiply")
                        ),
                        ListLink(
                            GroundedObjectNode("a", 6),
                            GroundedObjectNode("b", 7)
                        )
                    )

        result = execute_atom(self.space, exec_link)

        self.assertEqual(result.get_object(), 42)

    def test_set_object_without_arguments_wrapping(self):
        grounded_object_node = GroundedObjectNode("obj", TestObject("some object"))
        grounded_object_node.set_object(TestObject("other object"),
                                        unwrap_args = True)

        exec_link = ApplyLink(
                        MethodOfLink(
                            grounded_object_node,
                            ConceptNode("plain_multiply")
                        ),
                        ListLink(
                            GroundedObjectNode("a", 6),
                            GroundedObjectNode("b", 7)
                        )
                    )

        result = execute_atom(self.space, exec_link)

        self.assertEqual(result.get_object(), 42)

    def test_nested_call_without_arguments_wrapping(self):
        obj = GroundedObjectNode("obj", TestObject("obj"), unwrap_args = True)
        exec_link = ApplyLink(
                        MethodOfLink(obj, ConceptNode("plain_multiply")),
                        ListLink(
                            GroundedObjectNode("a", 6),
                            ApplyLink(
                                MethodOfLink(obj, ConceptNode("plain_sum")),
                                ListLink(
                                    GroundedObjectNode("b", 3),
                                    GroundedObjectNode("c", 4)
                                )
                            )
                        )
                    )

        result = execute_atom(self.space, exec_link)

        self.assertEqual(result.get_object(), 42)

    def test_create_grounded_object_node_without_object(self):
        first = GroundedObjectNode("obj", TestObject("obj"))
        second = GroundedObjectNode("obj")

        self.assertEqual(second, first)

    def test_call_from_pattern_matcher(self):
        first = GroundedObjectNode("first", TestObject("first"))
        second = GroundedObjectNode("second", TestObject("second"))
        bind_link = BindLink(
                        TypedVariableLink(VariableNode("$X"),
                            TypeNode("GroundedObjectNode")),
                        AndLink(
                            EvaluationLink(
                                GroundedPredicateNode("py: return_true"),
                                ApplyLink(
                                    MethodOfLink(VariableNode("$X"),
                                        ConceptNode("get_argument")),
                                    ListLink(ConceptNode("result"))
                                )
                            )
                        ),
                        VariableNode("$X")
                    )

        result = execute_atom(self.space, bind_link)

        self.assertEqual(result, SetLink(first, second))

    def test_get_object(self):
        obj = TestObject("some object")
        gon = GroundedObjectNode("a", obj)
        refc = sys.getrefcount(obj)

        name = gon.get_object().name
        self.assertEqual(refc, sys.getrefcount(obj))
    def test_pass_correctly_wrapped_args_to_python_grounded_predicate_node(self):
        obj = GroundedObjectNode("obj", TestObject("some obj"), unwrap_args = True)
        bindlink = BindLink(
                TypedVariableLink(VariableNode("$X"),
                                  TypeNode("GroundedObjectNode")),
                AndLink(
                    EvaluationLink(
                        GroundedPredicateNode("py: float_to_truth_value"),
                        ApplyLink(MethodOfLink(VariableNode("$X"),
                                               ConceptNode("truth")),
                                  ListLink()))
                ),
                VariableNode("$X")
        )

        result = execute_atom(self.space, bindlink)

        self.assertEqual(result, SetLink(obj))

def return_true(atom):
    return TruthValue(1.0, 1.0)

def float_to_truth_value(atom):
    return TruthValue(*atom.get_object())

__main__.return_true = return_true
__main__.float_to_truth_value = float_to_truth_value

class TestObject:

    def __init__(self, name):
        self.name = name

    def get_argument(self, arg):
        return arg

    def no_arguments(self):
        return ConceptNode("empty")

    def get_second(self, first, second):
        return second

    def plain_sum(self, a, b):
        return a + b

    def plain_multiply(self, a, b):
        return a * b

    def truth(self):
        return (1.0, 1.0)

if __name__ == '__main__':
    unittest.main()
