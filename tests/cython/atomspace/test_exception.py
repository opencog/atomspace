import unittest
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *
from opencog.bindlink import evaluate_atom
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

    # This test is trying to make sure that python doesn't
    # crash when a C++ exception is thrown.
    def test_bogus_evaluation(self):
        atom1 = ConceptNode("atom1")
        eval_link = EvaluationLink(GroundedPredicateNode("py:foobar"),
                                        atom1, atom1, atom1)
        try:
           evaluate_atom(self.space, eval_link)
           self.assertFalse("call should fail")
        except RuntimeError as e:
           self.assertTrue("bad evaluation link" in str(e))
