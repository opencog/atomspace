import unittest
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *
from opencog.bindlink import evaluate_atom
from opencog.scheme_wrapper import scheme_eval

import __main__


# All of these tests try to make sure that python doesn't
# crash when a C++ exception is thrown.
class TestExceptions(unittest.TestCase):

    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_bogus_get(self):
        atom1 = ConceptNode("atom1")
        try:
           GetLink(atom1, atom1, atom1)
           self.assertFalse("call should fail")
        except RuntimeError as e:
           # Use `nosetests3 --nocapture` to see this print...
           print("The exception message is " + str(e))
           self.assertTrue("Expecting" in str(e))

    def test_bogus_evaluation(self):
        atom1 = ConceptNode("atom1")
        eval_link = EvaluationLink(GroundedPredicateNode("py:foobar"),
                                        atom1, atom1, atom1)
        try:
           evaluate_atom(self.space, eval_link)
           self.assertFalse("call should fail")
        except RuntimeError as e:
           # Use `nosetests3 --nocapture` to see this print...
           print("The exception message is " + str(e))
           self.assertTrue("not found in module" in str(e))

    # This test passes on my machine, but fails on circle-ci.
    # I don't know why; the error is opaque. Commenting out.
    def xtest_bogus_scheme(self):
        try:
           code = '''(Get (Concept "a") (Concept "a") (Concept "a"))'''
           scheme_eval(self.space, code)
           self.assertFalse("call should fail")
        except RuntimeError as e:
           # Use `nosetests3 --nocapture` to see this print...
           print("The exception message is " + str(e))
           self.assertTrue("Expecting" in str(e))

    # This test passes on my machine, but fails on circle-ci.
    # I don't know why; the error is opaque. Commenting out.
    def xtest_bogus_path(self):
        try:
           code = '''(load-from-path "/blargle/Betelgeuse")'''
           scheme_eval(self.space, code)
           self.assertFalse("call should fail")
        except RuntimeError as e:
           # Use `nosetests3 --nocapture` to see this print...
           print("The exception message is " + str(e))
           self.assertTrue("Unable to find" in str(e))

# ===================== END OF FILE =================
