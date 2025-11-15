import unittest
import os

from opencog.atomspace import Atom, types

from opencog.type_constructors import *
from opencog.type_ctors import get_thread_atomspace

from test_functions import green_count, red_count
import test_functions

__author__ = 'Curtis Faith'


class BindlinkTest(unittest.TestCase):

    bindlink_atom = None
    getlink_atom = None
    starting_size = 0

    def setUp(self):
        self.atomspace = get_thread_atomspace()
        print ("setUp - atomspace = ", self.atomspace)

        # Clear atoms from previous test
        self.atomspace.clear()

        # Define several animals and something of a different type as well
        InheritanceLink( ConceptNode("Frog"),       ConceptNode("animal"))
        InheritanceLink( ConceptNode("Zebra"),      ConceptNode("animal"))
        InheritanceLink( ConceptNode("Deer"),       ConceptNode("animal"))
        InheritanceLink( ConceptNode("Spaceship"),  ConceptNode("machine"))

        # Define a graph search query
        self.bindlink_atom =  \
                QueryLink(
                    # The variable node to be grounded.
                    VariableNode("$var"),

                    # The pattern to be grounded.
                    InheritanceLink(
                        VariableNode("$var"),
                        ConceptNode("animal")
                    ),

                    # The grounding to be returned.
                    VariableNode("$var")
                # querylink needs a handle
                )

        # Define a pattern to be grounded
        self.getlink_atom =  \
            MeetLink(
                InheritanceLink(
                    VariableNode("$var"),
                    ConceptNode("animal")
                )
            )

        # Remember the starting atomspace size.
        self.starting_size = self.atomspace.size()

    def tearDown(self):
        print ("tearDown - atomspace = ", self.atomspace)

    def _check_result_setlink(self, value, expected_arity):

        # Check if the value is a UnisetValue
        self.assertTrue(value is not None)
        # UnisetValue is returned as a Value, check it has content
        value_list = value.to_list()
        self.assertTrue(isinstance(value_list, list))

        # Check the ending atomspace size, it should be unchanged (no SetLink created)
        ending_size = self.atomspace.size()
        self.assertEqual(ending_size, self.starting_size)

        # The UnisetValue should have expected_arity items in it.
        self.assertEqual(len(value_list), expected_arity)

    def test_bindlink(self):
        value = self.bindlink_atom.execute()
        print(f"QueryLink found: {str(value)}")
        self._check_result_setlink(value, 3)

    def test_satisfying_set(self):
        value = self.getlink_atom.execute()
        self._check_result_setlink(value, 3)

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

        tv = satisfaction_atom.execute()
        self.assertEqual(green_count(), 2)
        self.assertEqual(red_count(), 1)

    def test_execute_atom(self):
        result = ExecutionOutputLink(
                    GroundedSchemaNode("py: test_functions.add_link"),
                    ListLink(
                        ConceptNode("one"),
                        ConceptNode("two")
                    )
                ).execute()
        list_link = ListLink(
                ConceptNode("one"),
                ConceptNode("two")
            )
        self.assertEqual(result, list_link)

    def test_evaluate_atom(self):
        result = EvaluationLink(
                    GroundedPredicateNode("py: test_functions.bogus_tv"),
                    ListLink(
                        ConceptNode("one"),
                        ConceptNode("two")
                    )
                ).execute()
        self.assertEqual(result, BoolValue(True))

    def test_execute_atom_no_return_value(self):
        result = PutLink(DeleteLink(VariableNode("X")),
                        ConceptNode("deleteme")).execute()
        self.assertEqual(result, None)


    def test_tmp_atomspace(self):
        ListLink(ConceptNode("foo"), ConceptNode("bar"))

        get = MeetLink(VariableNode("x"),
                AndLink(
                    PresentLink(ListLink (ConceptNode("foo"),
                                          (VariableNode("x")))),
                    EvaluationLink(GroundedPredicateNode("py: test_functions.func_one"),
                        ListLink(VariableNode("x"))),
                   EvaluationLink(GroundedPredicateNode( "py: test_functions.func_two"),
                   ListLink (VariableNode ("x")))))
        result = get.execute()
        self.assertFalse(result.to_list())
        self.assertFalse(self.atomspace.is_node_in_atomspace(types.ConceptNode, 'barleycorn'))
        test_functions.func_one_result = FloatValue([1,1,1])
        result = get.execute()
        self.assertTrue(result.to_list())
        # still should not be in the current namespace
        self.assertFalse(self.atomspace.is_node_in_atomspace(types.ConceptNode, 'barleycorn'))

if __name__ == "__main__":
    unittest.main()
