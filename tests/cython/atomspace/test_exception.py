import unittest
from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *
from opencog.bindlink import evaluate_atom

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

    # First, make sure that evaluation works.
    def test_good_evaluation(self):
        atom1 = ConceptNode("atom1")
        eval_link = EvaluationLink(GroundedPredicateNode("py:good_tv"),
                                        atom1, atom1, atom1)
        okay = evaluate_atom(self.space, eval_link)

        # Use `nosetests3 --nocapture` to see this print...
        print("The good TV is " + str(okay))
        expect = TruthValue(0.5, 0.5)
        self.assertTrue(okay == expect)

    # Call a non-existent function.
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

    def test_pass_evaluation(self):
        atom1 = ConceptNode("atom1")
        eval_link = EvaluationLink(GroundedPredicateNode("py:no_ret"),
                                        atom1, atom1, atom1)
        try:
            evaluate_atom(self.space, eval_link)
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print("The exception message is " + str(e))
            self.assertTrue("did not return a TruthValue" in str(e))

    def test_num_evaluation(self):
        atom1 = ConceptNode("atom1")
        eval_link = EvaluationLink(GroundedPredicateNode("py:ret_num"),
                                        atom1, atom1, atom1)
        try:
            evaluate_atom(self.space, eval_link)
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print("The exception message is " + str(e))
            self.assertTrue("did not return a TruthValue" in str(e))

    def test_str_evaluation(self):
        atom1 = ConceptNode("atom1")
        eval_link = EvaluationLink(GroundedPredicateNode("py:ret_str"),
                                        atom1, atom1, atom1)
        try:
            evaluate_atom(self.space, eval_link)
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print("The exception message is " + str(e))
            self.assertTrue("did not return a TruthValue" in str(e))

    def test_nil_evaluation(self):
        atom1 = ConceptNode("atom1")
        eval_link = EvaluationLink(GroundedPredicateNode("py:ret_nil"),
                                        atom1, atom1, atom1)
        try:
            evaluate_atom(self.space, eval_link)
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print("The exception message is " + str(e))
            self.assertTrue("did not return a TruthValue" in str(e))

    def test_lst_evaluation(self):
        atom1 = ConceptNode("atom1")
        eval_link = EvaluationLink(GroundedPredicateNode("py:ret_lst"),
                                        atom1, atom1, atom1)
        try:
            evaluate_atom(self.space, eval_link)
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print("The exception message is " + str(e))
            self.assertTrue("did not return a TruthValue" in str(e))


def good_tv(*args):
    print(args)
    return TruthValue(0.5, 0.5)

def no_ret(*args):
    print(args)
    pass

def ret_num(*args):
    print(args)
    42

def ret_str(*args):
    print(args)
    "My name is Jon Jonson, I come from Wisconsin"

def ret_nil(*args):
    print(args)
    []

def ret_lst(*args):
    print(args)
    ['a', 'b', 'c']

__main__.good_tv = good_tv
__main__.no_ret = no_ret
__main__.ret_num = ret_num
__main__.ret_str = ret_str
__main__.ret_nil = ret_nil
__main__.ret_lst = ret_lst

if __name__ == '__main__':
    unittest.main()

# ===================== END OF FILE =================
