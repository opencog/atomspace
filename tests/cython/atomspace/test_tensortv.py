from unittest import TestCase

from opencog.bindlink import execute_atom
from opencog.atomspace import TTruthValue
from opencog.atomspace import AtomSpace, types
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *

from torch import tensor
import torch


class TensorTVTest(TestCase):

    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def test_tv(self):
        v = TTruthValue(0.4, 0.5)
        delta = 0.0000001
        self.assertTrue(float(v.mean) - 0.4 < delta)
        self.assertTrue(float(v.confidence) - 0.5 < delta)
        self.assertTrue(isinstance(eval(str(v)), TTruthValue))

        c = ConceptNode('c')
        c.tv = v

        t = torch.Tensor([0.2, 0.2])
        s1 = (t + c.tv.torch())
        s2 = (c.tv.torch() + t)
        self.assertTrue(str(s1) == str(s2))

        res = (execute_atom(atomspace, StrengthOfLink(c)))
        self.assertTrue(res.to_list()[0] - 0.4 < delta)

    def tearDown(self):
        finalize_opencog()
