import unittest

from opencog.atomspace import AtomSpace
from opencog.type_constructors import *
from opencog.scheme import scheme_eval

import __main__

# All of these tests try to make sure that python doesn't
# crash when a C++ exception is thrown.
class TestExceptions(unittest.TestCase):
    def setUp(self):
        self.space = AtomSpace()

    def tearDown(self):
        del self.space

    def test_bogus_scheme(self):
        try:
            code = """(Meet (Concept "a") (Concept "a") (Concept "a"))"""
            scheme_eval(self.space, code)
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("Expecting" in str(e))

    def test_bogus_path(self):
        try:
            code = """(load-from-path "/blargle/Betelgeuse")"""
            scheme_eval(self.space, code)
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("Unable to find" in str(e))


# ===================== END OF FILE =================
