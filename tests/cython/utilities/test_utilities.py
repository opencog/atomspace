import os
import random
import tempfile
import filecmp

from unittest import TestCase

from opencog.type_constructors import *
from opencog.atomspace import AtomSpace
from opencog.utilities import initialize_opencog, finalize_opencog, is_closed, get_free_variables

__author__ = 'Curtis Faith'


def write_sorted_file(path, atomspace):
    with open(path, 'wt') as f:
        for atom in sorted(list(atomspace)):
            f.write(str(atom))
            f.write('\n')


class UtilitiesTest(TestCase):

    def setUp(self):
        self.atomspace = AtomSpace()
 
    def tearDown(self):
        del self.atomspace

    def test_initialize_finalize(self):
        initialize_opencog(self.atomspace)
        finalize_opencog()

    def test_is_closed(self):
        A = self.atomspace.add_node(types.ConceptNode, 'A')
        B = self.atomspace.add_node(types.ConceptNode, 'B')
        X = self.atomspace.add_node(types.VariableNode, '$X')
        AB = self.atomspace.add_link(types.InheritanceLink, [A, B])
        AX = self.atomspace.add_link(types.InheritanceLink, [A, X])
        self.assertTrue(is_closed(AB))
        self.assertFalse(is_closed(AX))

    def test_get_free_variables(self):
        A = self.atomspace.add_node(types.ConceptNode, 'A')
        X = self.atomspace.add_node(types.VariableNode, '$X')
        AX = self.atomspace.add_link(types.InheritanceLink, [A, X])
        self.assertEqual(get_free_variables(AX), [X])


def gen_name():
    tmp = []
    ascii = [chr(x) for x in range(32, 127)]
    for _ in range(10):
        char = random.choice(ascii)
        if char == '"':
            char = '\\"'
        if char == '\\':
            char = ''
        tmp.append(char)
    return ''.join(tmp)


def gen_atoms(atomspace, num=100000):
    predicates = [
        atomspace.add_node(types.PredicateNode, f'predicate{str(x)}')
        for x in range(1)
    ]
    concepts = [
        atomspace.add_node(types.ConceptNode, f'concept{gen_name()}')
        for _ in range(1000)
    ]
    link_types = [types.ListLink, types.InheritanceLink, types.MemberLink]
    while(len(atomspace) < num):
        c1 = random.choice(concepts)
        c2 = random.choice(concepts)
        if c1 == c2:
            continue
        link_type = random.choice(link_types)
        arg = atomspace.add_link(link_type, [c1, c2])
        predicate = random.choice(predicates)
        atomspace.add_link(types.EvaluationLink,
                [predicate,
                arg])
    return atomspace
