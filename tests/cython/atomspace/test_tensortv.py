from unittest import TestCase

from opencog.bindlink import execute_atom
from opencog.atomspace import AtomSpace
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *


torch_found = True
try:
    from opencog.atomspace import TensorTruthValue
    from torch import tensor
    import torch
except ImportError as e:
    print("torch not found")
    torch_found = False


class TensorTVTest(TestCase):

    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def test_tv(self):
        if not torch_found:
            return
        v = TensorTruthValue(0.4, 0.5)
        delta = 0.0000001
        self.assertTrue(float(v.mean) - 0.4 < delta)
        self.assertTrue(float(v.confidence) - 0.5 < delta)
        self.assertTrue(isinstance(eval(str(v)), TensorTruthValue))

        c = ConceptNode('c')
        c.tv = v

        t = torch.Tensor([0.2, 0.2])
        s1 = (t + c.tv.torch())
        s2 = (c.tv.torch() + t)
        self.assertTrue(str(s1) == str(s2))

        res = (execute_atom(self.space, StrengthOfLink(c)))
        self.assertTrue(res.to_list()[0] - 0.4 < delta)
        self.assertTrue(c.tv.count == 1.0)

    def tearDown(self):
        finalize_opencog()
