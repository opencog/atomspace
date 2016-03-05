from unittest import TestCase
import os

from opencog.atomspace import AtomSpace, TruthValue, Atom, types
from opencog.bindlink import stub_bindlink, bindlink, single_bindlink,\
                             first_n_bindlink, af_bindlink, \
                             satisfaction_link, satisfying_set, \
                             satisfying_element, first_n_satisfying_set, \
                             execute_atom, evaluate_atom

from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *

from test_functions import green_count, red_count

__author__ = 'Curtis Faith'


class BindlinkTest(TestCase):

    bindlink_atom = None
    getlink_atom = None
    atomspace = AtomSpace()
    starting_size = 0

    def setUp(self):
        print "setUp - atomspace = ", self.atomspace

        # Clear atoms from previous test
        self.atomspace.clear()

        # Get the config file name in a manner not dependent on the
        # starting working directory.
        full_path = os.path.realpath(__file__)
        config_file_name = os.path.dirname(full_path) + "/bindlink_test.conf"

        # Initialize Python
        initialize_opencog(self.atomspace, config_file_name)
        set_type_ctor_atomspace(self.atomspace)

        # Define several animals and something of a different type as well
        InheritanceLink( ConceptNode("Frog"),       ConceptNode("animal"))
        InheritanceLink( ConceptNode("Zebra"),      ConceptNode("animal"))
        InheritanceLink( ConceptNode("Deer"),       ConceptNode("animal"))
        InheritanceLink( ConceptNode("Spaceship"),  ConceptNode("machine"))

        # Define a graph search query
        self.bindlink_atom =  \
                BindLink(
                    # The variable node to be grounded.
                    VariableNode("$var"),

                    # The pattern to be grounded.
                    InheritanceLink(
                        VariableNode("$var"),
                        ConceptNode("animal")
                    ),

                    # The grounding to be returned.
                    VariableNode("$var")
                # bindlink needs a handle
                )

        # Define a pattern to be grounded
        self.getlink_atom =  \
            GetLink(
                InheritanceLink(
                    VariableNode("$var"),
                    ConceptNode("animal")
                )
            )

        # Remember the starting atomspace size.
        self.starting_size = self.atomspace.size()


    def tearDown(self):
        print "tearDown - atomspace = ", self.atomspace

        # Can't do this; finalize can be called only once, ever, and
        # then never again.  The second call will never follow through.
        # Also, cannot create and delete atomspaces here; this will
        # confuse the PythonEval singletonInstance.
        # finalize_opencog()
        # del self.atomspace

    def test_stub_bindlink(self):

        # Remember the starting atomspace size. This test should not
        # change the atomspace.
        starting_size = self.atomspace.size()

        # Run bindlink.
        atom = stub_bindlink(self.atomspace, self.bindlink_atom)
        self.assertTrue(atom is not None and atom.value() > 0)

        # Check the ending atomspace size, it should be the same.
        ending_size = self.atomspace.size()
        self.assertEquals(ending_size, starting_size)

    def _check_result_setlink(self, atom, expected_arity):

        # Check if the atom is a SetLink
        self.assertTrue(atom is not None and atom.value() > 0)
        self.assertEquals(atom.type, types.SetLink)

        # Check the ending atomspace size, it should have added one SetLink.
        ending_size = self.atomspace.size()
        self.assertEquals(ending_size, self.starting_size + 1)

        # The SetLink should have expected_arity items in it.
        self.assertEquals(atom.arity, expected_arity)

    def test_bindlink(self):
        atom = bindlink(self.atomspace, self.bindlink_atom)
        self._check_result_setlink(atom, 3)

    def test_single_bindlink(self):
        atom = single_bindlink(self.atomspace, self.bindlink_atom)
        self._check_result_setlink(atom, 1)

    def test_first_n_bindlink(self):
        atom = first_n_bindlink(self.atomspace, self.bindlink_atom, 5)
        self._check_result_setlink(atom, 3)

    def test_af_bindlink(self):
        atom = af_bindlink(self.atomspace, self.bindlink_atom)
        # The SetLink is empty. ??? Should it be.
        self._check_result_setlink(atom, 0)

    def test_satisfying_set(self):
        atom = satisfying_set(self.atomspace, self.getlink_atom)
        self._check_result_setlink(atom, 3)

    def test_satisfying_element(self):
        atom = satisfying_element(self.atomspace, self.getlink_atom)
        self._check_result_setlink(atom, 1)

    def test_first_n_satisfying_set(self):
        atom = first_n_satisfying_set(self.atomspace, self.getlink_atom, 5)
        self._check_result_setlink(atom, 3)

    def test_satisfy(self):
        satisfaction_atom = SatisfactionLink(
            VariableList(),  # no variables
            SequentialAndLink(
                EvaluationLink(
                    GroundedPredicateNode("py: test_functions.stop_go"),
                    ListLink(
                        ConceptNode("green light")
                    )
                ),
                EvaluationLink(
                    GroundedPredicateNode("py: test_functions.stop_go"),
                    ListLink(
                        ConceptNode("green light")
                    )
                ),
                EvaluationLink(
                    GroundedPredicateNode("py: test_functions.stop_go"),
                    ListLink(
                        ConceptNode("red light")
                    )
                ),
                EvaluationLink(
                    GroundedPredicateNode("py: test_functions.stop_go"),
                    ListLink(
                        ConceptNode("traffic ticket")
                    )
                )
            )
        )

        atom = satisfaction_link(self.atomspace, satisfaction_atom)
        self.assertTrue(atom is not None and atom.mean <= 0.5)
        self.assertEquals(green_count(), 2)
        self.assertEquals(red_count(), 1)

    def test_execute_atom(self):
        result = execute_atom(self.atomspace,
                ExecutionOutputLink(
                    GroundedSchemaNode("py: test_functions.add_link"),
                    ListLink(
                        ConceptNode("one"),
                        ConceptNode("two")
                    )
                )
            )
        list_link = ListLink(
                ConceptNode("one"),
                ConceptNode("two")
            )
        self.assertEquals(result, list_link)

    def test_evaluate_atom(self):
        result = evaluate_atom(self.atomspace,
                EvaluationLink(
                    GroundedPredicateNode("py: test_functions.bogus_tv"),
                    ListLink(
                        ConceptNode("one"),
                        ConceptNode("two")
                    )
                )
            )
        self.assertEquals(result, TruthValue(0.6, 0.234))
