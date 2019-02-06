import os
import unittest
from opencog.forwardchainer import ForwardChainer
from opencog.scheme_wrapper import load_scm, scheme_eval
from opencog.type_constructors import *
from opencog.type_constructors import *
from opencog.utilities import initialize_opencog
from unittest import TestCase

from opencog.atomspace import TruthValue


class FCTest(TestCase):

    def setUp(self):
        self.atomspace = AtomSpace()
        initialize_opencog(self.atomspace)

        scheme_eval(atomspace, '(add-to-load-path "../../..")')
        scheme_eval(atomspace, '(add-to-load-path "../../../opencog/scm/opencog/rule-engine")')
        scm_dir = os.environ["SCM_DIR"]
        load_scm(self.atomspace, scm_dir + "/fc-deduction-config.scm")

    def test_modus_ponens(self):
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
    os.environ["SCM_DIR"] = "../../rule-engine/forwardchainer/scm"
    unittest.main()
