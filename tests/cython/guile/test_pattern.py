from unittest import TestCase

from opencog.atomspace import AtomSpace, TruthValue, Atom
from opencog.atomspace import types, is_a, get_type, get_type_name
from opencog.scheme_wrapper import load_scm, scheme_eval, scheme_eval_h


# We are poking atoms into this from the scm files, so we want
# them to still be there, later.
shared_space = AtomSpace()

class SchemeTest(TestCase):

    def setUp(self):
        global shared_space
        self.space = shared_space

    def tearDown(self):
        pass

    # Load several different scheme files, containing atom type
    # declarations, and utilities. They should load just fine.
    # These don't actually put any atoms into the atomspace.

    def test_a_load_core_types(self):

        scheme_eval(self.space, "(use-modules (opencog))")


    # Load a file that results in atoms placed in the atomspace.
    # Make sure the loaded atom is what we think it is.
    def test_b_load_file(self):

        status = load_scm(self.space, "tests/cython/guile/basic_unify.scm")
        self.assertTrue(status)

        a1 = self.space.add_node(types.ConceptNode, "hello")
        self.assertTrue(a1)

        # Make sure the truth value is what's in the SCM file.
        expected = TruthValue(0.5, 0.5)
        self.assertEquals(a1.tv, expected)
        # print a1.tv, expected

    # Run some basic evaluation tests
    def test_c_eval(self):
        basic = scheme_eval_h(self.space,
            "(ConceptNode \"whatever\" (stv 0.5 0.5))")

        a1 = self.space.add_node(types.ConceptNode, "whatever")
        self.assertTrue(a1)

        # Make sure the truth value is what's in the SCM file.
        expected = TruthValue(0.5, 0.5)
        self.assertEquals(a1.tv, expected)

        # Actually, the atoms overall should compare.
        self.assertEquals(a1, basic)

        # Do it again, from a define in the scm file.
        again = scheme_eval_h(self.space, "wobbly")
        a2 = self.space.add_node(types.ConceptNode, "wobbly")
        self.assertTrue(a2)
        self.assertEquals(a2, again)


    # Run the pattern-matcher/unifier/query-engine.
    def test_unifier(self):

        scheme_eval(self.space, "(use-modules (opencog query))")
        question = scheme_eval_h(self.space, "find-animals")
        self.assertTrue(question)
        print "\nThe question is:"
        print question

        answer = scheme_eval_h(self.space, "(cog-bind find-animals)")
        self.assertTrue(answer)
        print "\nThe answer is:"
        print answer
        self.assertEqual(answer.type, types.SetLink)
        self.assertEqual(answer.arity, 3)
