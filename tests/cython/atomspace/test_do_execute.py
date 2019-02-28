import unittest

from opencog.type_constructors import *
from opencog.bindlink import execute_atom
from opencog.utilities import initialize_opencog, finalize_opencog


class DoExecuteTest(unittest.TestCase):

    def setUp(self):
        self.atomspace = AtomSpace()
        initialize_opencog(self.atomspace)

    def tearDown(self):
        finalize_opencog()
        del self.atomspace

    def test_do_execute_value(self):
        key = PredicateNode("key")
        atom = ConceptNode("atom")
        atom.set_value(key, FloatValue([1, 2, 3]))

        value_of_link = ValueOfLink(atom, key)

        value = execute_atom(atomspace, value_of_link)
        self.assertEqual(FloatValue([1, 2, 3]), value)
        self.assertEqual([1, 2, 3], value.to_list())

    def test_do_execute_atom(self):
        (DefineLink(
            DefinedSchemaNode('add'),
            LambdaLink(
                VariableList(
                    VariableNode('$X'),
                    VariableNode('$Y')),
                PlusLink(
                    VariableNode('$X'),
                    VariableNode('$Y')))))

        res = execute_atom(self.atomspace,
                           ExecutionOutputLink(
                               DefinedSchemaNode('add'),
                               ListLink(
                                   NumberNode("3"),
                                   NumberNode("4"))))
        self.assertEqual(NumberNode("7"), res)


if __name__ == '__main__':
    unittest.main()
