import unittest

from opencog.atomspace import AtomSpace
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.atomspace import PtrValue

class PtrValueTest(unittest.TestCase):

    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_call_grounded_object_predicate(self):
        obj = TestObject("some object")

        value = PtrValue(obj)

        ref = value.value()
        self.assertEqual(ref.name, "some object")

class TestObject:
    def __init__(self, name):
        self.name = name
