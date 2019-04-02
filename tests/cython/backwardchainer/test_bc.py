import os
import unittest
from unittest import TestCase
from opencog.scheme_wrapper import scheme_eval
from opencog.atomspace import TruthValue
from opencog.ure import BackwardChainer
from opencog.type_constructors import *
from opencog.utilities import initialize_opencog, finalize_opencog
import __main__


def run_predicate(a, b):
    print(a.name)
    print(b.name)
    if a.name == "Item0" and b.name == "large":
        return TruthValue(0.8, 1.0)
    if a.name == "Item1" and b.name == "small":
        return TruthValue(0.8, 1.0)
    return TruthValue(0.2, 0.8)


__main__.run_predicate = run_predicate


class BCTest(TestCase):

    def setUp(self):
        self.atomspace = AtomSpace()
        initialize_opencog(self.atomspace)

    def tearDown(self):
        finalize_opencog()
        del self.atomspace

    def init(self):
        project_source_dir = os.environ["PROJECT_SOURCE_DIR"]
        scheme_eval(self.atomspace, '(add-to-load-path "{0}")'.format(project_source_dir))
        scheme_eval(self.atomspace,
                    '(add-to-load-path "{0}/{1}")'.format(project_source_dir, "tests/ure/backwardchainer/scm"))

    def test_bc_deduction(self):
        """port of crisp.scm from examples/ure/simple"""

        print ("Enter test_bc_deduction")
        self.init()

        print ("test_bc_deduction: pre scheme eval")
        scheme_eval(self.atomspace, '(use-modules (opencog))')
        scheme_eval(self.atomspace, '(use-modules (opencog exec))')
        scheme_eval(self.atomspace, '(use-modules (opencog ure))')
        print ("test_bc_deduction: pre scheme load")
        scheme_eval(self.atomspace, '(load-from-path "bc-deduction-config.scm")')
        print ("test_bc_deduction: post scheme load")

        A = ConceptNode("A", TruthValue(1, 1))
        B = ConceptNode("B")
        C = ConceptNode("C")
        AB = InheritanceLink(A, B)
        AB.tv = TruthValue(1, 1)
        BC = InheritanceLink(B, C)
        BC.tv = TruthValue(1, 1)
        crisprbs = ConceptNode("crisp-rule-base")
        # InheritanceLink(crisprbs, ConceptNode("URE"))
        print ("test_bc_deduction: pre backchain ctor")
        chainer = BackwardChainer(self.atomspace,
                                  ConceptNode("URE"),
                                  InheritanceLink(VariableNode("$who"), C),
                                  TypedVariableLink(VariableNode("$who"), TypeNode("ConceptNode")))

        scheme_eval(self.atomspace, '(gc)')
        print ("test_bc_deduction: post backchain ctor")
        chainer.do_chain()
        print ("test_bc_deduction: post chaining")
        scheme_eval(self.atomspace, '(gc)')
        results = chainer.get_results()
        print ("test_bc_deduction: post results")
        scheme_eval(self.atomspace, '(gc)')
        resultAC = None
        for result in results.get_out():
            if result.get_out()[0].name == "A":
                resultAC = result
                break
        self.assertTrue(resultAC is not None)
        self.assertTrue(resultAC.tv == AB.tv)
        self.assertEquals("A", resultAC.get_out()[0].name)
        self.assertEquals("C", resultAC.get_out()[1].name)
        del chainer

    def test_conjunction_fuzzy_with_virtual_evaluation(self):
        """Test for correct vardecl parameter initialization in BackwardChainer

        vardecl = VariableList() would cause empty set as backward chaining result
        vardecl = UNDEFINED should produce two results: (Item0, large), (Item1, small)
        """

        self.init()

        scheme_eval(self.atomspace, '(load-from-path "fuzzy-conjunction-introduction-config.scm")')
        rbs = ConceptNode("conjunction-rule-base")

        item0 = ConceptNode("Item0")
        item1 = ConceptNode("Item1")
        size = ConceptNode("Size")
        small = ConceptNode("small")
        large = ConceptNode("large")
        InheritanceLink(item0, ConceptNode("Items")).tv = TruthValue(0.9, 0.9)
        InheritanceLink(item1, ConceptNode("Items")).tv = TruthValue(0.9, 0.9)
        InheritanceLink(small, size).tv = TruthValue(1.0, 1.0)
        InheritanceLink(large, size).tv = TruthValue(1.0, 1.0)
        P = GroundedPredicateNode("py:run_predicate")
        X = VariableNode("$X")
        Y = VariableNode("$Y")
        P_A = EvaluationLink(P, ListLink(X, Y))
        target = AndLink(InheritanceLink(Y, size), InheritanceLink(X, ConceptNode("Items")), P_A)

        bc = BackwardChainer(self.atomspace, rbs, target)
        bc.do_chain()
        results = bc.get_results()
        self.assertTrue(len(results.get_out()) == 2)
        del bc


if __name__ == '__main__':
    os.environ["PROJECT_SOURCE_DIR"] = "../../.."
    unittest.main()
