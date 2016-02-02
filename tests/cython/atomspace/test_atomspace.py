from unittest import TestCase

from opencog.atomspace import AtomSpace, TruthValue, Atom
from opencog.atomspace import types, is_a, get_type, get_type_name

from opencog.utilities import initialize_opencog, finalize_opencog
from opencog.type_constructors import *

class AtomSpaceTest(TestCase):

    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

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
        # check with raw UUID
        self.assertTrue(self.space.is_valid(a1.value()))
        # check with bad UUID
        self.assertFalse(self.space.is_valid(2919))
        # check with bad type
        self.assertRaises(TypeError, self.space.is_valid, "test")

    def test_truth_value(self):
        # check attributes come back as assigned
        tv = TruthValue(0.5, 0.8)
        self.assertEqual(tv.mean, 0.5)
        self.assertAlmostEqual(tv.confidence, 0.8, places=4)
        # test string representation
        self.assertEqual(str(tv), "(stv 0.500000 0.800000)")

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

    def test_attention_value(self):
        node = Node("test")

        # check values come back as assigned
        node.sti = 1
        node.lti = 2
        node.vlti = 3
        assert node.sti == 1
        assert node.lti == 2
        assert node.vlti == 3

        # Check increment and decrement for vlti
        node.decrement_vlti()
        assert node.vlti == 2
        node.increment_vlti()
        assert node.vlti == 3

        # Check dictionary setting and getting of av property.
        node.av = {"sti": 4, "lti": 5, "vlti": 6}
        assert node.sti == 4
        assert node.lti == 5
        assert node.vlti == 6
        assert node.av == {"sti": 4, "lti": 5, "vlti": 6}

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

    def test_get_by_av(self):
        a1 = ConceptNode("test1")
        a2 = ConceptNode("test2")
        a3 = InheritanceLink(a1, a2)
        a4 = ConceptNode("test4")
        a5 = ConceptNode("test5")

        a1.sti = 10
        a2.sti = 5
        a3.sti = 4
        a4.sti = 1

        result = self.space.get_atoms_by_av(4, 10)
        assert len(result) == 3
        assert set(result) == set([a1, a2, a3])
        assert a4 not in result

        result = self.space.get_atoms_in_attentional_focus()
        assert len(result) == 4
        assert set(result) == set([a1, a2, a3, a4])

    def test_incoming_by_type(self):
        a1 = Node("test1")
        a2 = ConceptNode("test2")
        a3 = PredicateNode("test3")

        # test no incoming Node for a1
        result = a1.incoming_by_type(types.Node)
        self.assertTrue(a1 not in result)

        # now check links
        l1 = InheritanceLink(a1, a2)
        result = a1.incoming_by_type(types.Link)
        self.assertTrue(l1 in result)
        result = a2.incoming_by_type(types.Link)
        self.assertTrue(l1 in result)
        result = a3.incoming_by_type(types.Link)
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

    def test_get_predicates(self):
        dog = ConceptNode("dog")
        mammal = ConceptNode("mammal")
        canine = ConceptNode("canine")
        animal = ConceptNode("animal")
        dog_mammal = ListLink(dog, mammal)
        dog_canine = ListLink(dog, canine)
        dog_animal = ListLink(dog, animal)
        isA = PredicateNode("IsA")
        dogIsAMammal = EvaluationLink(isA, dog_mammal)
        dogIsACanine = EvaluationLink(isA, dog_canine)
        dogIsAAnimal = EvaluationLink(isA, dog_animal)

        dog_predicates = self.space.get_predicates(dog)
        self.assertEquals(len(dog_predicates), 3)

        count = 0
        for dogIs in self.space.xget_predicates(dog):
            count += 1
        self.assertEquals(count, 3)

    def test_get_predicates_for(self):
        dog = ConceptNode("dog")
        mammal = ConceptNode("mammal")
        canine = ConceptNode("canine")
        animal = ConceptNode("animal")
        dog_mammal = ListLink(dog, mammal)
        dog_canine = ListLink(dog, canine)
        dog_animal = ListLink(dog, animal)
        isA = PredicateNode("IsA")
        dogIsAMammal = EvaluationLink(isA, dog_mammal)
        dogIsACanine = EvaluationLink(isA, dog_canine)
        dogIsAAnimal = EvaluationLink(isA, dog_animal)

        human = ConceptNode("human")
        dog_human = ListLink(dog, human)
        loves = PredicateNode("loves")
        dogLovesHumans = EvaluationLink(loves, dog_human)

        dog_predicates = self.space.get_predicates_for(dog, isA)
        self.assertEquals(len(dog_predicates), 3)

        dog_predicates = self.space.get_predicates_for(dog, loves)
        self.assertEquals(len(dog_predicates), 1)

        count = 0
        for dogIsA in self.space.xget_predicates_for(dog, isA):
            count += 1
        self.assertEquals(count, 3)

        count = 0
        for dogLoves in self.space.xget_predicates_for(dog, loves):
            count += 1
        self.assertEquals(count, 1)

class AtomTest(TestCase):

    def setUp(self):
        self.space = AtomSpace()
        initialize_opencog(self.space)

    def tearDown(self):
        finalize_opencog()
        del self.space

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
        
    def test_w_attention_value(self):
        a = Node("test2")

        self.assertEqual(a.av, {'lti': 0, 'sti': 0, 'vlti': False})

        # test set av
        a.av = { "sti": 10, "lti": 1, "vlti": True }
        self.assertEqual(a.av, {'sti': 10, 'lti': 1, 'vlti': True})

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

        self.assertEqual(a.type, 1)
        self.assertEqual(a.t, 1)

        a2 = Node("test3")
        l = Link(a, a2)
        self.assertEqual(l.type, 2)
        self.assertEqual(l.t, 2)

        # ensure type is considered immutable
        self.assertRaises(AttributeError, setattr, l, "type", 5)
        self.assertRaises(AttributeError, setattr, a, "type", 5)

        self.assertEqual(l.type_name, "Link")
        self.assertEqual(a.type_name, "Node")


    def test_strings(self):
        # set up a link and atoms
        tv = TruthValue(0.5, 0.8)
        a1 = Node("test1", tv)

        a2 = Node("test2")
        a2.av = {"sti": 10, "lti": 1, "vlti": True}
        a2.tv = TruthValue(0.1, 0.3)

        l = Link(a1, a2)

        space_uuid = self.space.uuid

        # test string representation
        a1_expected = "(Node \"test1\") ; [{0}][{1}]\n".format(str(a1.value()), space_uuid)
        a1_expected_long = \
            "(Node \"test1\" (stv 0.500000 0.800000)) ; [{0}][{1}]\n"\
            .format(str(a1.value()), space_uuid)

        a2_expected = "(Node \"test2\") ; [{0}][{1}]\n".format(str(a2.value()), space_uuid)
        a2_expected_long = \
            "(Node \"test2\" (av 10 1 1) (stv 0.100000 0.300000)) ; [{0}][{1}]\n"\
            .format(str(a2.value()), space_uuid)

        l_expected = \
            "(Link\n  {0}  {1}) ; [{2}][{3}]\n"\
            .format(a1_expected, a2_expected, str(l.value()), space_uuid)
        l_expected_long = \
            "(Link\n  {0}  {1}) ; [{2}][{3}]\n"\
            .format(a1_expected_long, a2_expected_long, str(l.value()), space_uuid)

        self.assertEqual(str(a1), a1_expected)
        self.assertEqual(a1.long_string(), a1_expected_long)
        self.assertEqual(str(a2), a2_expected)
        self.assertEqual(a2.long_string(), a2_expected_long)
        self.assertEqual(str(l), l_expected)
        self.assertEqual(l.long_string(), l_expected_long)

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
        self.assertEqual(get_type_name(types.NO_TYPE), "")
