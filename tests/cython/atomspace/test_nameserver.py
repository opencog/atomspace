import unittest

from opencog.atomspace import (types, decl_type, type_decl_context, AtomSpace,
        get_type_name)
from opencog.utilities import add_node, add_link, push_default_atomspace, pop_default_atomspace
from opencog.type_constructors import *

# Types must be declared once per module
with type_decl_context(__name__):
    decl_type(types.Node, 'SomeNode')
    decl_type(types.Link, 'SomeLink')

def SomeNode(name, tv=None):
    return add_node(types.SomeNode, name, tv)

def SomeLink(*args, tv=None):
    return add_link(types.SomeLink, args, tv)

class NameserverTest(unittest.TestCase):

    def setUp(self):
        self.space = AtomSpace()
        push_default_atomspace(self.space)

    def tearDown(self):
        pop_default_atomspace()

    def _get_atoms_by_type(self, type):
        type_name = get_type_name(type)
        return Get(VariableList(
            TypedVariable(Variable("X"), Type(type_name))),
            And(Variable("X"))).execute()

    def test_decl_node(self):
        node = SomeNode("test")
        res = self._get_atoms_by_type(types.SomeNode)
        self.assertEqual(res.out, [ node ])

    def test_decl_link(self):
        link = SomeLink(SomeNode("a"), SomeNode("b"))
        res = self._get_atoms_by_type(types.SomeLink)
        self.assertEqual(res.out, [ link ])

if __name__ == '__main__':
    unittest.main()
