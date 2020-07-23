from unittest import TestCase

from opencog.atomspace import AtomSpace, Atom
from opencog.type_constructors import TruthValue
from opencog.atomspace import types, is_a, get_type, get_type_name
from opencog.scheme import scheme_eval, scheme_eval_h
import os


# We are poking atoms into this from the scm files, so we want
# them to still be there, later.
shared_space = AtomSpace()

class SchemeTest(TestCase):

    def setUp(self):
        global shared_space
        self.space = shared_space
        scheme_eval(self.space, '(add-to-load-path "' +
                    os.environ['PROJECT_SOURCE_DIR'] + '")')
        scheme_eval(self.space, '(add-to-load-path "' +
                    os.environ['PROJECT_SOURCE_DIR'] + '/opencog/scm")')

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

        print("Enter load-file test\n")
        status = scheme_eval(self.space, '(load-from-path "tests/cython/guile/basic_unify.scm")')
        self.assertTrue(status)

        print("Loaded file\n")
        a1 = self.space.add_node(types.ConceptNode, "hello")
        self.assertTrue(a1)

        print("Added atom\n")
        # Make sure the truth value is what's in the SCM file.
        expected = TruthValue(0.5, 0.5)
        self.assertEquals(a1.tv, expected)
        print("Got=" + str(a1.tv) + " expected=" + str(expected))

    # Create lots of large, random strings, try to trick guile gc
    # into running, while in the python context. We want to make
    # sure that gc works while we are in the python interpreter.
    # Guile gc uses the SIGPWR and SIGXCPU signals, which seems
    # to sometimes manifest in strange circle-ci failures!? ???
    def test_c_gc(self):
        print("Enter garbage-collection-test\n")
        status = scheme_eval(self.space, '(define n 0)')
        self.assertTrue(status)
        status = scheme_eval(self.space, """
            (for-each
                (lambda (y)
                    (let* ((bigstr (list->string (map
                                (lambda (x)
                                    (integer->char (+ 48 (modulo (+ x y) 79))))
                                (iota 900))))
                           (biglst (string->list bigstr))
                           (revstr (reverse-list->string biglst)))
                        (set! n (+ 1 n))))
                    (iota 2000))""")
        self.assertTrue(status)
        status = scheme_eval(self.space, '(gc-stats)')
        self.assertTrue(status)
        print("Finish garbage-collection-test\n")

    # Run some basic evaluation tests
    def test_d_eval(self):
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

        scheme_eval(self.space, "(use-modules (opencog exec))")
        question = scheme_eval_h(self.space, "find-animals")
        self.assertTrue(question)
        print ("\nThe question is:", question)

        answer = scheme_eval_h(self.space, "(cog-execute! find-animals)")
        self.assertTrue(answer)
        print ("\nThe answer is:", answer)
        self.assertEqual(answer.type, types.SetLink)
        self.assertEqual(answer.arity, 3)
