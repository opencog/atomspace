import os
from unittest import TestCase
from opencog.scheme_wrapper import scheme_eval
from opencog.atomspace import TruthValue
from opencog.backwardchainer import BackwardChainer
from opencog.type_constructors import *
from opencog.utilities import initialize_opencog
from opencog.scheme_wrapper import load_scm
import opencog.logger
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
        scheme_eval(self.atomspace, '(use-modules (opencog))')
        scheme_eval(self.atomspace, '(use-modules (opencog exec))')
        scheme_eval(self.atomspace, '(use-modules (opencog query))')
        scheme_eval(self.atomspace, '(use-modules (opencog logger))')
        scheme_eval(self.atomspace, '(use-modules (opencog rule-engine))')
        scm_dir = os.environ["SCM_DIR"]
        scheme_eval(self.atomspace, '(add-to-load-path "{0}")'.format(scm_dir))

    def test_crisp(self):
        """port of crisp.scm from examples/rule-engine/simple"""
        scheme_eval(self.atomspace, '(load-from-path "crisp-config.scm")')
        A = PredicateNode("A", TruthValue(1, 1))
        B = PredicateNode("B")
        C = PredicateNode("C")
        AB = ImplicationLink(A, B)
        AB.tv = TruthValue(1, 1)
        BC = ImplicationLink(B, C)
        BC.tv = TruthValue(1, 1)
        crisprbs = ConceptNode("crisp-rule-base")
        InheritanceLink(crisprbs, ConceptNode("URE"))
        trace_as = AtomSpace()
        scheme_eval(self.atomspace, '(crisp-fc (ImplicationLink (stv 1 1) (PredicateNode "A") (PredicateNode "B")))')
        chainer = BackwardChainer(self.atomspace, crisprbs, C)
        chainer.do_chain()
        results = chainer.get_results()
        self.assertTrue(results.get_out()[0].name == "C")
        self.assertTrue(results.get_out()[0].tv == AB.tv)

    def test_conjunction_fuzzy_with_virtual_evaluation(self):
        """Test for correct vardecl parameter initialization in BackwardChainer

	vardecl = VariableList() would cause empty set as backward chaining result
        vardecl = UNDEFINED should produce two results: (Item0, large), (Item1, small)
	"""
        scheme_eval(self.atomspace, '(load-from-path "conjunction-rule-base-config.scm")')
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
