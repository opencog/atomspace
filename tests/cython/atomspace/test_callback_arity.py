import unittest
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *
from opencog.bindlink import execute_atom
import __main__


def return_concept(atom):
    return atom.atomspace.add_node(types.ConceptNode, "test")


__main__.return_concept = return_concept


class TestExecutionOutputLink(unittest.TestCase):

    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_correct_argcount(self):
        atom1 = ConceptNode("atom1")
        exec_link = ExecutionOutputLink(GroundedSchemaNode("py:return_concept"),
                                        ListLink(atom1))
        result = execute_atom(self.space, exec_link)

    def test_incorrect_argcount(self):
        atom1 = ConceptNode("atom1")
        exec_link = ExecutionOutputLink(GroundedSchemaNode("py:return_concept"),
                                        ListLink(atom1, atom1))
        try:
           result = execute_atom(self.space, exec_link)
           self.assertFalse("call should fail")
        except RuntimeError:
           pass

    def test_incorrect_atomspace_init(self):
        self.space = AtomSpace()
        try:
           initialize_opencog(self.space, 15)
        except TypeError:
           return
        self.assertFalse("call should fail with TypeError")

