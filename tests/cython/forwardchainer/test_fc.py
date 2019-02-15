import os
import unittest
from unittest import TestCase

from opencog.ure import ForwardChainer
from opencog.scheme_wrapper import load_scm, scheme_eval
from opencog.type_constructors import *
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.atomspace import TruthValue


class FCTest(TestCase):

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
                    '(add-to-load-path "{0}/{1}")'.format(project_source_dir, "tests/rule-engine/forwardchainer/scm"))
        scheme_eval(self.atomspace,
                    '(add-to-load-path "{0}/{1}")'.format(project_source_dir, "opencog/scm/opencog/rule-engine"))

    def test_forward_chainer_instantiation(self):
        chainer = ForwardChainer(self.atomspace,
                                 ConceptNode("PLN"),
                                 SetLink())
        self.assertIsNotNone(chainer)

    def test_fc_deduction(self):
        self.init()
        scheme_eval(self.atomspace, '(load-from-path "fc-deduction-config.scm")')

        A = ConceptNode("A")
        B = ConceptNode("B")
        C = ConceptNode("C")

        InheritanceLink(A, B).tv = TruthValue(0.8, 0.9)
        InheritanceLink(B, C).tv = TruthValue(0.98, 0.94)

        chainer = ForwardChainer(self.atomspace,
                                 ConceptNode("fc-deduction-rule-base"),
                                 InheritanceLink(VariableNode("$who"), C),
                                 TypedVariableLink(VariableNode("$who"), TypeNode("ConceptNode")))
        chainer.do_chain()
        results = chainer.get_results()

        resultLink = results.out[0]
        self.assertEquals(types.InheritanceLink, resultLink.type)

        resultA = resultLink.out[0]
        resultC = resultLink.out[1]
        self.assertEquals(A, resultA)
        self.assertEquals(C, resultC)

        resultTV = resultLink.tv
        self.assertAlmostEqual(1.0, resultTV.mean, places=5)
        self.assertAlmostEqual(1.0, resultTV.confidence, places=5)


if __name__ == '__main__':
    os.environ["PROJECT_SOURCE_DIR"] = "../../.."
    unittest.main()
