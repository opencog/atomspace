import os
import random
import tempfile
import filecmp

from unittest import TestCase

from opencog.type_constructors import *
from opencog.atomspace import AtomSpace
from opencog.utilities import initialize_opencog, finalize_opencog, load_file, is_closed

__author__ = 'Curtis Faith'


def write_sorted_file(path, atomspace):
    with open(path, 'wt') as f:
        for atom in sorted([x for x in atomspace]):
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

    def test_fast_load(self):
        gen_atoms(self.atomspace)
        with tempfile.TemporaryDirectory() as tmpdirname:
            tmp_file = os.path.join(tmpdirname, 'tmp.scm')
            write_sorted_file(tmp_file, self.atomspace)
            new_space = AtomSpace()
            load_file(tmp_file, new_space)
            self.assertTrue(len(new_space) == len(self.atomspace))
            # files should be binary equal
            new_tmp = os.path.join(tmpdirname, 'tmp1.scm')
            write_sorted_file(new_tmp, new_space)
            self.assertTrue(filecmp.cmp(tmp_file, new_tmp, shallow=False), "files are not equal")
            checklist = """(ListLink(ConceptNode "vfjv\\"jnvfé")
                (ConceptNode "conceptIR~~gF\\",KV")
                (ConceptNode "вверху плыли редкие облачка"))"""
            with open(tmp_file, 'wt') as f:
                f.write(checklist)
            new_space1 = AtomSpace()
            load_file(tmp_file, new_space1)
            self.assertTrue(len(new_space1) == 4)

    def test_is_closed(self):
        A = self.atomspace.add_node(types.ConceptNode, 'A')
        B = self.atomspace.add_node(types.ConceptNode, 'B')
        X = self.atomspace.add_node(types.VariableNode, '$X')
        AB = self.atomspace.add_link(types.InheritanceLink, [A, B])
        AX = self.atomspace.add_link(types.InheritanceLink, [A, X])
        self.assertTrue(is_closed(AB))
        self.assertFalse(is_closed(AX))


def gen_name():
    tmp = []
    ascii = [chr(x) for x in range(32, 127)]
    for i in range(10):
        char = random.choice(ascii)
        if char == '"':
            char = '\\"'
        if char == '\\':
            char = ''
        tmp.append(char)
    return ''.join(tmp)


def gen_atoms(atomspace, num=100000):
    predicates = [atomspace.add_node(types.PredicateNode, 'predicate' + str(x)) for x in range(1)]
    concepts = [atomspace.add_node(types.ConceptNode, 'concept' + gen_name()) for x in range(1000)]
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
