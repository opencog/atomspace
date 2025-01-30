from unittest import TestCase

import opencog.atomspace
from opencog.atomspace import Atom
from opencog.atomspace import types, is_a, get_type, get_type_name, create_child_atomspace

from opencog.type_constructors import *
from opencog.utilities import initialize_opencog, finalize_opencog, tmp_atomspace

from time import sleep

class AtomSpaceTest(TestCase):

    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_bare(self):

        # Test dereferencing self as Value
        self.space
        self.space.type
        self.space.long_string()
        self.space.short_string()
        self.space.is_atom()
        self.space.is_node()
        self.space.is_link()
        list(self.space)
        str(self.space)
        len(self.space)

    def test_add_node(self):

        # Test long form atomspace node addition.

        # Test node add
        self.space.add_node(types.Node, "node" )

        # Test with not a proper truthvalue
        self.assertRaises(TypeError, self.space.add_node, types.Node, "test",
                0, True)
        # Test with bad type
        self.assertRaises(TypeError, self.space.add_node, "ConceptNode", "test",
                TruthValue(0.5, 0.8))

        # From here on out we'll use the more compact type constructors
        a1 = Node("test")
        self.assertTrue(a1)
        # duplicates resolve to same atom
        a2 = Node("test")
        self.assertEquals(a1, a2)

        # Should fail when intentionally adding bad type
        caught = False
        try:
            self.space.add_node(types.Link, "test")
        except RuntimeError:
            caught = True
        self.assertEquals(caught, True)

        # Test adding with a truthvalue
        a3 = Node("test_w_tv").truth_value(0.5, 0.8)
        self.assertEquals(self.space.size(), 3)

        # Test alternative way of adding with a truthvalue
        a4 = Node("test_w_tv_alt", tv=TruthValue(0.5, 0.8))
        self.assertEquals(self.space.size(), 4)

    def test_add_link(self):
        n1 = Node("test1")
        n2 = Node("test2")
        l1 = Link(n1, n2)
        self.assertTrue(l1 is not None)
        l2 = Link(n1, n2)
        self.assertTrue(l2 is not None)
        self.assertTrue(l2 == l1)

        n3 = Node("test3")
        l3 = Link(n1, n3).truth_value(0.5, 0.8)
        self.assertTrue(l3 is not None)

        n4 = Node("test4")
        l4 = Link(n1, n4, tv=TruthValue(0.5, 0.8))
        self.assertTrue(l4 is not None)

        # Should fail when adding an intentionally bad type
        caught = False
        try:
            l1 = self.space.add_link(types.Node, [n1, n3])
        except RuntimeError:
            caught = True
        self.assertEquals(caught, True)

    def test_is_valid(self):
        a1 = Node("test1")
        # check with Atom object
        self.assertTrue(self.space.is_valid(a1))
        # check with bad type
        self.assertRaises(TypeError, self.space.is_valid, "test")

    def test_truth_value(self):
        # check attributes come back as assigned
        tv = TruthValue(0.5, 0.8)
        self.assertEqual(tv.mean, 0.5)
        self.assertAlmostEqual(tv.confidence, 0.8, places=4)
        # test string representation
        self.assertEqual(str(tv), "(stv 0.5 0.8)")

        # check equality
        tv2 = TruthValue(0.5, 0.8)
        tv3 = TruthValue(0.6, 0.8)
        self.assertTrue(tv == tv2)
        self.assertFalse(tv == tv3)

        # check truth_value function of atom
        atom = Node("atom with tv")
        default_tv = atom.tv
        atom.truth_value(0.75, 0.9)
        new_tv = atom.tv
        self.assertFalse(new_tv == default_tv)
        self.assertEqual(new_tv.mean, 0.75)
        self.assertAlmostEqual(new_tv.confidence, 0.9, places=4)

    def test_get_by_type(self):
        a1 = Node("test1")
        a2 = ConceptNode("test2")
        a3 = PredicateNode("test3")

        # test recursive subtypes
        result = self.space.get_atoms_by_type(types.Node)
        self.assertTrue(a1 in result)
        self.assertTrue(a2 in result)
        self.assertTrue(a3 in result)

        # links
        l1 = InheritanceLink(a1, a2)
        result = self.space.get_atoms_by_type(types.Link)
        self.assertTrue(l1 in result)

        # test non-recursive subtype
        result = self.space.get_atoms_by_type(types.Node, subtype=False)
        self.assertTrue(a1 in result)
        self.assertTrue(a2 not in result)
        self.assertTrue(a3 not in result)

        # test empty
        result = self.space.get_atoms_by_type(types.AnchorNode, subtype=False)
        self.assertEqual(len(result), 0)

    def test_incoming_by_type(self):
        a1 = Node("test1")
        a2 = ConceptNode("test2")
        a3 = PredicateNode("test3")

        # test no incoming Node for a1
        result = a1.incoming_by_type(types.Node)
        self.assertTrue(a1 not in result)

        # now check links
        l1 = InheritanceLink(a1, a2)
        result = a1.incoming_by_type(types.InheritanceLink)
        self.assertTrue(l1 in result)
        result = a2.incoming_by_type(types.InheritanceLink)
        self.assertTrue(l1 in result)
        result = a3.incoming_by_type(types.InheritanceLink)
        self.assertTrue(l1 not in result)

    def test_include_incoming_outgoing(self):
        frog = ConceptNode("Frog")
        thing = ConceptNode("Thing")
        animal = ConceptNode("Animal")
        ConceptNode("SeparateThing")
        InheritanceLink(frog, animal)
        InheritanceLink(animal, thing)

        assert len(self.space.include_incoming([ConceptNode("Frog")])) == 2
        assert len(self.space.include_outgoing(self.space.include_incoming([ConceptNode("Frog")]))) == 3
        assert len(self.space.include_incoming(self.space.get_atoms_by_type(types.ConceptNode))) == 6
        assert len(self.space.include_outgoing(self.space.get_atoms_by_type(types.InheritanceLink))) == 5

    def test_remove(self):
        a1 = Node("test1")
        a2 = ConceptNode("test2")
        a3 = PredicateNode("test3")

        self.assertTrue(a1 in self.space)
        self.assertTrue(a2 in self.space)
        self.assertTrue(a3 in self.space)

        self.space.remove(a1)
        self.assertTrue(a1 not in self.space)
        self.assertTrue(a2 in self.space)
        self.assertTrue(a3 in self.space)

        l = SimilarityLink(a2, a3)
        self.space.remove(a2, True) # won't remove it unless recursive is True
        self.assertTrue(a2 not in self.space)
        self.assertTrue(l not in self.space)

    def test_clear(self):
        a1 = Node("test1")
        a2 = ConceptNode("test2")
        a3 = PredicateNode("test3")
        self.space.clear()
        self.assertEquals(self.space.size(), 0)
        self.assertEquals(len(self.space), 0)

    def test_container_methods(self):
        self.assertEquals(len(self.space), 0)
        a1 = Node("test1")
        a2 = ConceptNode("test2")
        a3 = PredicateNode("test3")

        self.assertTrue(a1 in self.space)
        self.assertTrue(a2 in self.space)
        self.assertTrue(a3 in self.space)

        self.assertEquals(len(self.space), 3)

    def test_context_mgr_tmp(self):
        a = ConceptNode('a')
        with tmp_atomspace() as tmp_as:
             b = ConceptNode('b')
             self.assertTrue(a in self.space)
             # verify that current default atomspace is tmp_as
             self.assertFalse(b in self.space)
        c = ConceptNode('c')
        # verify that current default atomspace is self.space
        self.assertTrue(c in self.space)


class AtomTest(TestCase):

    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

    def test_create_child_atomspace(self):
        """
        Test that parent atomspace will not be deleted before child
        """
        a = opencog.atomspace.AtomSpace()
        b = opencog.atomspace.create_child_atomspace(a)
        del a

    def test_creation(self):
        a = Node("test1")
        self.assertEqual(a.name, "test1")
        self.assertEqual(a.tv, TruthValue(1.0, 0.0)) # default is true, no confidence

    def test_w_truthvalue(self):
        tv = TruthValue(0.5, 100)
        a = Node("test2", tv)
        self.assertEqual(a.tv, tv)

        # test set tv
        a.tv = TruthValue(0.1, 10)
        self.assertEqual(a.tv, TruthValue(0.1, 10))

    def test_out(self):
        # test get out
        a1 = Node("test2")

        self.assertEqual(a1.out, [])

        tv = TruthValue(0.5, 100)
        a2 = Node("test3", tv)

        l = Link(a1, a2)
        self.assertEqual(l.out, [a1, a2])

        # ensure out is considered immutable
        self.assertRaises(AttributeError, setattr, l, "out", [a1])

    def test_arity(self):
        a1 = Node("test2")

        self.assertEqual(a1.arity, 0)

        tv = TruthValue(0.5, 100)
        a2 = Node("test3", tv)

        l = Link(a1, a2)
        self.assertEqual(l.arity, 2)

        # ensure arity is considered immutable
        self.assertRaises(AttributeError, setattr, l, "arity", 4)

    def test_type(self):
        # test get out
        a = Node("test2")
        a2 = Node("test3")
        l = Link(a, a2)

        # ensure type is considered immutable
        self.assertRaises(AttributeError, setattr, l, "type", 5)
        self.assertRaises(AttributeError, setattr, a, "type", 5)

        self.assertEqual(l.type_name, "Link")
        self.assertEqual(a.type_name, "Node")

    def test_create_child_atomspace(self):
        test = ConceptNode("test")
        b = create_child_atomspace(self.space)
        test2 = b.add_node(types.ConceptNode, 'test2')
        self.assertTrue(test in b.get_atoms_by_type(types.ConceptNode))
        self.assertTrue(test2 in b.get_atoms_by_type(types.ConceptNode))
        self.assertTrue(test2 not in self.space.get_atoms_by_type(types.ConceptNode))

    def test_strings(self):
        # set up a link and atoms
        tv = TruthValue(0.5, 0.8)
        a1 = Node("test1", tv)

        a2 = Node("test2")
        a2.tv = TruthValue(0.1, 0.3)

        l = Link(a1, a2)

        space_uuid = 0

        # test string representation
        a1_expected = "(Node \"test1\") ; [{0}]\n".format(space_uuid)
        a1_expected_long = \
            "(Node \"test1\" (stv 0.500000 0.800000)) ; [{0}]\n"\
            .format(space_uuid)

        a2_expected = "(Node \"test2\") ; [{0}]\n".format(space_uuid)
        a2_expected_long = \
            "(Node \"test2\" (stv 0.100000 0.300000)) ; [{0}]\n"\
            .format(space_uuid)

        l_expected = \
            "(Link\n  {0}  {1}) ; [{2}]\n"\
            .format(a1_expected, a2_expected, space_uuid)
        l_expected_long = \
            "(Link\n  {0}  {1}) ; [{2}]\n"\
            .format(a1_expected_long, a2_expected_long, space_uuid)

        # This just won't work as designed.
        #self.assertEqual(str(a1), a1_expected)
        #self.assertEqual(a1.long_string(), a1_expected_long)
        #self.assertEqual(str(a2), a2_expected)
        #self.assertEqual(a2.long_string(), a2_expected_long)
        #self.assertEqual(str(l), l_expected)
        #self.assertEqual(l.long_string(), l_expected_long)

class TypeTest(TestCase):

    def test_is_a(self):
        self.assertTrue(is_a(types.ConceptNode, types.Node))
        self.assertTrue(is_a(types.ConceptNode, types.Atom))

        self.assertTrue(is_a(types.ListLink, types.Link))
        self.assertTrue(is_a(types.ListLink, types.Atom))

        self.assertFalse(is_a(types.Link, types.Node))

    def test_get_type(self):
        self.assertEqual(get_type("ConceptNode"), types.ConceptNode)
        self.assertEqual(get_type(""), types.NO_TYPE)
        self.assertRaises(TypeError, get_type, 1)

    def test_get_type_name(self):
        self.assertEqual(get_type_name(types.Node), "Node")
        self.assertEqual(get_type_name(2231), "")
        # XXX FIXME is testing the name of the bottom type
        # a sane thing to do?
        self.assertEqual(get_type_name(types.NO_TYPE), "*** Bottom Type! ***")
