import unittest
import threading

from opencog.atomspace import create_child_atomspace
from opencog.type_constructors import *
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.utilities import push_default_atomspace, get_default_atomspace


class DoExecuteTest(unittest.TestCase):

    def setUp(self):
        self.atomspace = AtomSpace()
        initialize_opencog(self.atomspace)

    def tearDown(self):
        finalize_opencog()
        del self.atomspace

    def test_do_execute_value(self):
        key = PredicateNode("key")
        atom = ConceptNode("atom")
        atom.set_value(key, FloatValue([1, 2, 3]))

        value_of_link = ValueOfLink(atom, key)

        value = value_of_link.execute()
        self.assertEqual(FloatValue([1, 2, 3]), value)
        self.assertEqual([1, 2, 3], value.to_list())

    def test_do_execute_atom(self):
        (DefineLink(
            DefinedSchemaNode('add'),
            LambdaLink(
                VariableList(
                    VariableNode('$X'),
                    VariableNode('$Y')),
                PlusLink(
                    VariableNode('$X'),
                    VariableNode('$Y')))))

        res = ExecutionOutputLink(
                 DefinedSchemaNode('add'),
                 ListLink(
                     NumberNode("3"),
                     NumberNode("4"))).execute()
        self.assertEqual(NumberNode("7"), res)

    def test_add_atom_from_grounded_schema_node(self):
        test_as = create_child_atomspace(self.atomspace)
        test_as.execute(
                ExecutionOutputLink(
                    GroundedSchemaNode("py:add_new_link"),
                    ListLink()
                )
            )
        self.assertTrue(test_as.is_link_in_atomspace(types.InheritanceLink,
            [test_as.add_node(types.ConceptNode, "cat"),
            test_as.add_node(types.ConceptNode, "animal")]))

    def test_threaded(self):
        """push default atomspace in different thread and check the behaviour"""
        test_as = AtomSpace()
        different_as = AtomSpace()
        push_default_atomspace(test_as)
        th = threading.Thread(target=push_default, args=(different_as,))
        th.start()
        th.join()
        # ConceptNode('test-1') should only be in different_as
        # ConceptNode('test-2') should only be in test_as
        # if thread-local storage works as expected
        ConceptNode("test-2")
        self.assertTrue(different_as.is_node_in_atomspace(types.ConceptNode, "test-1"))
        self.assertFalse(different_as.is_node_in_atomspace(types.ConceptNode, "test-2"))


def push_default(atomspace):
    push_default_atomspace(atomspace)
    ConceptNode("test-1")


def add_new_link():
    return InheritanceLink(ConceptNode("cat"), ConceptNode("animal"))


import __main__
__main__.add_new_link = add_new_link


if __name__ == '__main__':
    unittest.main()
