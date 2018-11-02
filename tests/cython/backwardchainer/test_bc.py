import os
from unittest import TestCase
from opencog.scheme_wrapper import scheme_eval
from opencog.atomspace import TruthValue
from opencog.backwardchainer import BackwardChainer
from opencog.type_constructors import *
from opencog.utilities import initialize_opencog
from opencog.scheme_wrapper import load_scm


class CrispTest(TestCase):
    """port of crisp.scm from examples/rule-engine/simple"""

    def setUp(self):
        self.atomspace = AtomSpace()
        initialize_opencog(self.atomspace)
        scheme_eval(self.atomspace, '(use-modules (opencog))')
        scheme_eval(self.atomspace, '(use-modules (opencog exec))')
        scheme_eval(self.atomspace, '(use-modules (opencog query))')
        scheme_eval(self.atomspace, '(use-modules (opencog logger))')
        scm_dir = os.environ["SCM_DIR"]
        scheme_eval(self.atomspace, '(add-to-load-path "{0}")'.format(scm_dir))
        load_scm(self.atomspace, "crisp-config.scm")

    def testCrisp(self):
        A = PredicateNode("A", TruthValue(1, 1))
        B = PredicateNode("B")
        C = PredicateNode("C")
        AB = ImplicationLink(TruthValue(1, 1), A, B)
        BC = ImplicationLink(TruthValue(1, 1), B, C)
        BC = ImplicationLink(TruthValue(1, 1), B, C)
        crisprbs = ConceptNode("crisp-rule-base")
        InheritanceLink(crisprbs, ConceptNode("URE"))
        trace_as = AtomSpace()
        chainer = BackwardChainer(self.atomspace, crisprbs, C, trace_as=trace_as)
        chainer.do_chain()
        results = chainer.get_results()
        self.assertTrue(results.get_out()[0].name == "C")
