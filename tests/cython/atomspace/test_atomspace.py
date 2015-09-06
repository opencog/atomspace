from unittest import TestCase

from opencog.atomspace import AtomSpace, TruthValue, Atom, Handle
from opencog.atomspace import types, is_a, get_type, get_type_name

class AtomSpaceTest(TestCase):

    def setUp(self):
        self.space = AtomSpace()

    def tearDown(self):
        del self.space

    def test_add_node(self):

        a1 = self.space.add_node(types.Node, "test")
        self.assertTrue(a1)
        # duplicates resolve to same handle
        a2 = self.space.add_node(types.Node, "test")
        self.assertEquals(a1, a2)

        # Should fail when intentionally adding bad type
        # self.assertRaises(RuntimeError, self.space.add_node(types.Link, "test"))
        caught = False
        try:
            self.space.add_node(types.Link, "test")
        except RuntimeError:
            caught = True
        self.assertEquals(caught, True)

        # test adding with a truthvalue
        a3 = self.space.add_node(types.Node, "test_w_tv", TruthValue(0.5, 0.8))
        self.assertEquals(self.space.size(), 2)

        # tests with bad parameters
        # test with not a proper truthvalue
        self.assertRaises(TypeError, self.space.add_node, types.Node, "test", 0, True)
        # test with bad type
        self.assertRaises(TypeError, self.space.add_node, "ConceptNode",
"test", TruthValue(0.5, 0.8))

    def test_add_link(self):
        n1 = self.space.add_node(types.Node, "test1")
        n2 = self.space.add_node(types.Node, "test2")
        l1 = self.space.add_link(types.Link, [n1, n2])
        self.assertTrue(l1 is not None)
        l2 = self.space.add_link(types.Link, [n1, n2])
        self.assertTrue(l2 is not None)
        self.assertTrue(l2 == l1)

        n3 = self.space.add_node(types.Node, "test3")
        l3 = self.space.add_link(types.Link, [n1, n3], TruthValue(0.5, 0.8))
        self.assertTrue(l3 is not None)
        # Test with a handle instead of atom
        l4 = self.space.add_link(types.Link, [n2.h, n3], TruthValue(0.5, 0.8))
        self.assertTrue(l4 is not None)

        # Should fail when adding an intentionally bad type
        caught = False
        try:
            l1 = self.space.add_link(types.Node, [n1, n3])
        except RuntimeError:
            caught = True
        self.assertEquals(caught, True)

    def test_is_valid(self):
        h1 = self.space.add_node(types.Node, "test1")
        # check with Handle object
        self.assertTrue(self.space.is_valid(h1.h))
        # check with raw UUID
        self.assertTrue(self.space.is_valid(h1.h.value()))
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

    def test_get_by_name_and_type(self):
        n1 = self.space.add_node(types.Node, "test")
        n2 = self.space.add_node(types.ConceptNode, "test")
        n3 = self.space.add_node(types.PredicateNode, "test")

        # test recursive subtypes
        result = self.space.get_atoms_by_name(types.Node, "test")
        self.assertTrue(n1 in result)
        self.assertTrue(n2 in result)
        self.assertTrue(n3 in result)

        # test non-recursive subtype
        result = self.space.get_atoms_by_name(types.Node, "test", subtype=False)
        self.assertTrue(n1 in result)
        self.assertTrue(n2 not in result)
        self.assertTrue(n3 not in result)

        # test empty
        result = self.space.get_atoms_by_name(types.AnchorNode, "test", subtype=False)
        self.assertEqual(len(result), 0)

    def test_get_by_type(self):
        h1 = self.space.add_node(types.Node, "test1")
        h2 = self.space.add_node(types.ConceptNode, "test2")
        h3 = self.space.add_node(types.PredicateNode, "test3")

        # test recursive subtypes
        result = self.space.get_atoms_by_type(types.Node)
        self.assertTrue(h1 in result)
        self.assertTrue(h2 in result)
        self.assertTrue(h3 in result)

        # links
        l1 = self.space.add_link(types.InheritanceLink, [h1, h2])
        result = self.space.get_atoms_by_type(types.Link)
        self.assertTrue(l1 in result)
        
        # test non-recursive subtype
        result = self.space.get_atoms_by_type(types.Node, subtype=False)
        self.assertTrue(h1 in result)
        self.assertTrue(h2 not in result)
        self.assertTrue(h3 not in result)

        # test empty
        result = self.space.get_atoms_by_type(types.AnchorNode, subtype=False)
        self.assertEqual(len(result), 0)

    def test_get_by_av(self):
        h1 = self.space.add_node(types.ConceptNode, "test1")
        h2 = self.space.add_node(types.ConceptNode, "test2")
        h3 = self.space.add_link(types.InheritanceLink, [h1, h2])
        h4 = self.space.add_node(types.ConceptNode, "test4")
        h5 = self.space.add_node(types.ConceptNode, "test5")

        self.space.set_av(h=h1.h, sti=10)
        self.space.set_av(h=h2.h, sti=5)
        self.space.set_av(h=h3.h, sti=4)
        self.space.set_av(h=h4.h, sti=1)

        result = self.space.get_atoms_by_av(4, 10)
        assert len(result) == 3
        assert set(result) == set([h1, h2, h3])
        assert h4 not in result

        result = self.space.get_atoms_in_attentional_focus()
        assert len(result) == 4
        assert set(result) == set([h1, h2, h3, h4])

    def test_get_by_target_atom(self):
        h1 = self.space.add_node(types.Node, "test1")
        h2 = self.space.add_node(types.ConceptNode, "test2")
        h3 = self.space.add_node(types.PredicateNode, "test3")

        # test it doesn't apply to Nodes
        result = self.space.get_atoms_by_target_atom(types.Node, h1)
        self.assertTrue(h1 not in result)

        # links
        l1 = self.space.add_link(types.InheritanceLink, [h1, h2])
        result = self.space.get_atoms_by_target_atom(types.Link, h1)
        self.assertTrue(l1 in result)
        result = self.space.get_atoms_by_target_atom(types.Link, h3)
        self.assertTrue(l1 not in result)

    def test_include_incoming_outgoing(self):
        frog = self.space.add_node(types.ConceptNode, "Frog")
        thing = self.space.add_node(types.ConceptNode, "Thing")
        animal = self.space.add_node(types.ConceptNode, "Animal")
        self.space.add_node(types.ConceptNode, "SeparateThing")
        self.space.add_link(types.InheritanceLink, [frog, animal])
        self.space.add_link(types.InheritanceLink, [animal, thing])

        assert len(self.space.include_incoming(self.space.get_atoms_by_name(types.ConceptNode, "Frog"))) == 2
        assert len(self.space.include_incoming(self.space.get_atoms_by_type(types.ConceptNode))) == 6
        assert len(self.space.include_outgoing(self.space.get_atoms_by_type(types.InheritanceLink))) == 5
        assert len(self.space.include_outgoing(
            self.space.include_incoming(self.space.get_atoms_by_name(types.ConceptNode, "Frog")))) == 3

    def test_remove(self):
        h1 = self.space.add_node(types.Node, "test1")
        h2 = self.space.add_node(types.ConceptNode, "test2")
        h3 = self.space.add_node(types.PredicateNode, "test3")

        self.assertTrue(h1 in self.space)
        self.assertTrue(h2 in self.space)
        self.assertTrue(h3 in self.space)

        self.space.remove(h1)
        self.assertTrue(h1 not in self.space)
        self.assertTrue(h2 in self.space)
        self.assertTrue(h3 in self.space)

        l = self.space.add_link(types.SimilarityLink, [h2, h3])
        self.space.remove(h2, True) # won't remove it unless recursive is True
        self.assertTrue(h2 not in self.space)
        self.assertTrue(l not in self.space)

    def test_clear(self):
        h1 = self.space.add_node(types.Node, "test1")
        h2 = self.space.add_node(types.ConceptNode, "test2")
        h3 = self.space.add_node(types.PredicateNode, "test3")
        self.space.clear()
        self.assertEquals(self.space.size(), 0) 
        self.assertEquals(self.space.size(), 0) 
        self.assertEquals(len(self.space), 0) 

    def test_container_methods(self):
        self.assertEquals(len(self.space), 0) 
        h = Handle(100)
        self.assertRaises(KeyError, self.space.__getitem__, "blah")
        self.assertRaises(IndexError, self.space.__getitem__, h)
        a1 = self.space.add_node(types.Node, "test1")
        a2 = self.space.add_node(types.ConceptNode, "test2")
        a3 = self.space.add_node(types.PredicateNode, "test3")
        h1 = a1.h
        h2 = a2.h
        self.assertEquals(a1, self.space[h1])

        self.assertTrue(h1 in self.space)
        self.assertTrue(a1 in self.space)
        self.assertTrue(h2 in self.space)

        self.assertEquals(len(self.space), 3)

    def test_get_predicates(self):
        dog = self.space.add_node(types.ConceptNode, "dog")
        mammal = self.space.add_node(types.ConceptNode, "mammal")
        canine = self.space.add_node(types.ConceptNode, "canine")
        animal = self.space.add_node(types.ConceptNode, "animal")
        dog_mammal = self.space.add_link(types.ListLink, [dog, mammal])
        dog_canine = self.space.add_link(types.ListLink, [dog, canine])
        dog_animal = self.space.add_link(types.ListLink, [dog, animal])
        isA = self.space.add_node(types.PredicateNode, "IsA")
        dogIsAMammal = self.space.add_link(types.EvaluationLink, [isA, dog_mammal])
        dogIsACanine = self.space.add_link(types.EvaluationLink, [isA, dog_canine])
        dogIsAAnimal = self.space.add_link(types.EvaluationLink, [isA, dog_animal])

        dog_predicates = self.space.get_predicates(dog)
        self.assertEquals(len(dog_predicates), 3)

        count = 0
        for dogIs in self.space.xget_predicates(dog):
            count += 1
        self.assertEquals(count, 3)

    def test_get_predicates_for(self):
        dog = self.space.add_node(types.ConceptNode, "dog")
        mammal = self.space.add_node(types.ConceptNode, "mammal")
        canine = self.space.add_node(types.ConceptNode, "canine")
        animal = self.space.add_node(types.ConceptNode, "animal")
        dog_mammal = self.space.add_link(types.ListLink, [dog, mammal])
        dog_canine = self.space.add_link(types.ListLink, [dog, canine])
        dog_animal = self.space.add_link(types.ListLink, [dog, animal])
        isA = self.space.add_node(types.PredicateNode, "IsA")
        dogIsAMammal = self.space.add_link(types.EvaluationLink, [isA, dog_mammal])
        dogIsACanine = self.space.add_link(types.EvaluationLink, [isA, dog_canine])
        dogIsAAnimal = self.space.add_link(types.EvaluationLink, [isA, dog_animal])

        human = self.space.add_node(types.ConceptNode, "human")
        dog_human = self.space.add_link(types.ListLink, [dog, human])
        loves = self.space.add_node(types.PredicateNode, "loves")
        dogLovesHumans = self.space.add_link(types.EvaluationLink, [loves, dog_human])

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

    def test_creation(self):
        a = self.space.add_node(types.Node, "test1")
        self.assertEqual(a.name, "test1")
        self.assertEqual(a.tv, TruthValue(1.0, 0.0)) # default is true, no confidence

    def test_w_truthvalue(self):
        tv = TruthValue(0.5, 100)
        a = self.space.add_node(types.Node, "test2", tv)
        self.assertEqual(a.tv, tv)

        # test set tv
        a.tv = TruthValue(0.1, 10)
        self.assertEqual(a.tv, TruthValue(0.1, 10))
        
    def test_w_attention_value(self):
        a = self.space.add_node(types.Node, "test2")

        self.assertEqual(a.av, {'lti': 0, 'sti': 0, 'vlti': False})

        # test set av
        a.av = { "sti": 10, "lti": 1, "vlti": True }
        self.assertEqual(a.av, {'sti': 10, 'lti': 1, 'vlti': True})

    def test_out(self):
        # test get out
        a1 = self.space.add_node(types.Node, "test2")

        self.assertEqual(a1.out, [])

        tv = TruthValue(0.5, 100)
        a2 = self.space.add_node(types.Node, "test3", tv)

        l = self.space.add_link(types.Link, [a1, a2])
        self.assertEqual(l.out, [a1, a2])

        # ensure out is considered immutable
        self.assertRaises(AttributeError, setattr, l, "out", [a1])

    def test_arity(self):
        a1 = self.space.add_node(types.Node, "test2")

        self.assertEqual(a1.arity, 0)

        tv = TruthValue(0.5, 100)
        a2 = self.space.add_node(types.Node, "test3", tv)

        l = self.space.add_link(types.Link, [a1, a2])
        self.assertEqual(l.arity, 2)

        # ensure arity is considered immutable
        self.assertRaises(AttributeError, setattr, l, "arity", 4)

    def test_is_source(self):
        # any out item is a source for unordered links
        # only the fist item is a source of ordered links
        a1 = self.space.add_node(types.Node, "test1")
        a2 = self.space.add_node(types.Node, "test2")

        l_ordered = self.space.add_link(types.OrderedLink, [a1, a2])
        l_unordered = self.space.add_link(types.UnorderedLink, [a1, a2])

        self.assertEqual(l_ordered.is_source(a1), True)
        self.assertEqual(l_ordered.is_source(a2), False)

        self.assertEqual(l_unordered.is_source(a1), True)
        self.assertEqual(l_unordered.is_source(a2), True)

    def test_type(self):
        # test get out
        a = self.space.add_node(types.Node, "test2")

        self.assertEqual(a.type, 1)
        self.assertEqual(a.t, 1)

        a2 = self.space.add_node(types.Node, "test3")
        l = self.space.add_link(types.Link, [a, a2])
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
        a1 = self.space.add_node(types.Node, "test1", tv)

        a2 = self.space.add_node(types.Node, "test2")
        a2.av = {"sti": 10, "lti": 1, "vlti": True}
        a2.tv = TruthValue(0.1, 0.3)

        l = self.space.add_link(types.Link, [a1, a2])

        # test string representation
        a1_expected = "(Node \"test1\") ; [{0}][{1}]\n".format(str(a1.h.value()), 97)
        a1_expected_long = \
            "(Node \"test1\" (av 0 0 0) (stv 0.500000 0.800000)) ; [{0}]\n"\
            .format(str(a1.h.value()))

        a2_expected = "(Node \"test2\") ; [{0}][{1}]\n".format(str(a2.h.value()), 97)
        a2_expected_long = \
            "(Node \"test2\" (av 10 1 1) (stv 0.100000 0.300000)) ; [{0}]\n"\
            .format(str(a2.h.value()))

        l_expected = \
            "(Link (stv 1.000000 0.000000)\n  {0}  {1}) ; [{2}][{3}]\n"\
            .format(a1_expected, a2_expected, str(l.h.value()), 97)
        l_expected_long = \
            "(Link (av 0 0 0) (stv 1.000000 0.000000)\n  {0}  {1}) ; [{2}]\n"\
            .format(a1_expected_long, a2_expected_long, str(l.h.value()))

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
