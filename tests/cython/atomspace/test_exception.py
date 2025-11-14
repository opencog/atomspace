import unittest
from opencog.type_constructors import *

import __main__

# All of these tests try to make sure that python doesn't
# crash when a C++ exception is thrown.
class TestExceptions(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_bogus_get(self):
        atom1 = Concept("atom1")
        try:
            Meet(atom1, atom1, atom1)
            self.assertFalse("call should fail")
        except RuntimeError as e:
                   # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("Expecting" in str(e))

    # --------------------------------------------------------------
    # First, make sure that evaluation works.
    def test_good_evaluation(self):
        atom1 = Concept("atom1")
        eval_link = Evaluation(GroundedPredicate("py:good_predicate"),
                                        atom1, atom1, atom1)
        okay = eval_link.execute()

        # Use `nosetests3 --nocapture` to see this print...
        print(f"The good TV is {str(okay)}")
        expect = BoolValue(True)
        self.assertTrue(okay == expect)

    # --------------------------------------------------------------
    # Call a non-existent function.
    def test_bogus_evaluation(self):
        atom1 = Concept("atom1")
        eval_link = Evaluation(GroundedPredicate("py:foobar"),
                                        atom1, atom1, atom1)
        try:
            eval_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("not found in" in str(e))

    # Call function that returns None
    def test_pass_evaluation(self):
        atom1 = Concept("atom1")
        eval_link = Evaluation(GroundedPredicate("py:no_ret"),
                                        atom1, atom1, atom1)
        try:
            eval_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("did not return Atomese" in str(e))

    def test_num_evaluation(self):
        atom1 = Concept("atom1")
        eval_link = Evaluation(GroundedPredicate("py:ret_num"),
                                        atom1, atom1, atom1)
        try:
            eval_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("did not return Atomese" in str(e))

    def test_str_evaluation(self):
        atom1 = Concept("atom1")
        eval_link = Evaluation(GroundedPredicate("py:ret_str"),
                                        atom1, atom1, atom1)
        try:
            eval_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("did not return Atomese" in str(e))

    def test_nil_evaluation(self):
        atom1 = Concept("atom1")
        eval_link = Evaluation(GroundedPredicate("py:ret_nil"),
                                        atom1, atom1, atom1)
        try:
            eval_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("did not return Atomese" in str(e))

    def test_lst_evaluation(self):
        atom1 = Concept("atom1")
        eval_link = Evaluation(GroundedPredicate("py:ret_lst"),
                                        atom1, atom1, atom1)
        try:
            eval_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("did not return Atomese" in str(e))

    # --------------------------------------------------------------
    # First, make sure that evaluation works.
    def test_good_execution(self):
        atom1 = Concept("atom1")
        exec_link = ExecutionOutput(GroundedSchema("py:good_tv"),
                                        ListLink(atom1, atom1, atom1))
        okay = exec_link.execute()

        # Use `nosetests3 --nocapture` to see this print...
        print(f"The good TV is {str(okay)}")
        expect = FloatValue([0.5, 0.5])
        self.assertTrue(okay == expect)

    # --------------------------------------------------------------
    # Call a non-existent function.
    def test_bogus_execution(self):
        atom1 = Concept("atom1")
        exec_link = ExecutionOutput(GroundedSchema("py:foobar"),
                                        ListLink(atom1, atom1, atom1))
        try:
            exec_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("not found in" in str(e))

    # Call function that returns None
    def test_pass_execution(self):
        atom1 = Concept("atom1")
        exec_link = ExecutionOutput(GroundedSchema("py:no_ret"),
                                        ListLink(atom1, atom1, atom1))
        try:
            # exec_link.execute()
            exec_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("did not return Atomese" in str(e))

    def test_num_execution(self):
        atom1 = Concept("atom1")
        exec_link = ExecutionOutput(GroundedSchema("py:ret_num"),
                                        ListLink(atom1, atom1, atom1))
        try:
            # exec_link.execute()
            exec_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("did not return Atomese" in str(e))

    def test_str_execution(self):
        atom1 = Concept("atom1")
        exec_link = ExecutionOutput(GroundedSchema("py:ret_str"),
                                        ListLink(atom1, atom1, atom1))
        try:
            # exec_link.execute()
            exec_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("did not return Atomese" in str(e))

    def test_nil_execution(self):
        atom1 = Concept("atom1")
        exec_link = ExecutionOutput(GroundedSchema("py:ret_nil"),
                                        ListLink(atom1, atom1, atom1))
        try:
            # exec_link.execute()
            exec_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("did not return Atomese" in str(e))

    def test_lst_execution(self):
        atom1 = Concept("atom1")
        exec_link = ExecutionOutput(GroundedSchema("py:ret_lst"),
                                        ListLink(atom1, atom1, atom1))
        try:
            # exec_link.execute()
            exec_link.execute()
            self.assertFalse("call should fail")
        except RuntimeError as e:
            # Use `nosetests3 --nocapture` to see this print...
            print(f"The exception message is {str(e)}")
            self.assertTrue("did not return Atomese" in str(e))



def good_predicate(*args):
    print(args)
    return True

def good_tv(*args):
    print(args)
    return FloatValue([0.5, 0.5])

def no_ret(*args):
    print(args)

def ret_num(*args):
    print(args)
    return 42

def ret_str(*args):
    print(args)
    return "My name is Jon Jonson, I come from Wisconsin"

def ret_nil(*args):
    print(args)
    return {}

def ret_lst(*args):
    print(args)
    return ['a', 'b', 'c']

__main__.good_predicate = good_predicate
__main__.good_tv = good_tv
__main__.no_ret = no_ret
__main__.ret_num = ret_num
__main__.ret_str = ret_str
__main__.ret_nil = ret_nil
__main__.ret_lst = ret_lst

if __name__ == '__main__':
    unittest.main()

# ===================== END OF FILE =================
