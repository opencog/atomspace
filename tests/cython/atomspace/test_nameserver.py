import unittest

from opencog.atomspace import (types, decl_type, type_decl_context, AtomSpace,
        get_type_name)
from opencog.type_ctors import add_node, add_link, push_thread_atomspace, pop_thread_atomspace
from opencog.type_constructors import *

# Types must be declared once per module
with type_decl_context(__name__):
    decl_type(types.Node, 'SomeNode')
    decl_type(types.Link, 'SomeLink')

def SomeNode(name):
    return add_node(types.SomeNode, name)

def SomeLink(*args):
    return add_link(types.SomeLink, args)

class NameserverTest(unittest.TestCase):

    def setUp(self):
        self.atomspace = AtomSpace()
        push_thread_atomspace(self.atomspace)

    def tearDown(self):
        pop_thread_atomspace()

    def _get_atoms_by_type(self, type):
        type_name = get_type_name(type)
        return Meet(VariableList(
            TypedVariable(Variable("X"), Type(type_name))),
            And(Variable("X"))).execute()

    def test_decl_node(self):
        node = SomeNode("test")
        res = self._get_atoms_by_type(types.SomeNode)
        self.assertEqual(res.to_list(), [ node ])

    def test_decl_link(self):
        link = SomeLink(SomeNode("a"), SomeNode("b"))
        res = self._get_atoms_by_type(types.SomeLink)
        self.assertEqual(res.to_list(), [ link ])

if __name__ == '__main__':
    unittest.main()
