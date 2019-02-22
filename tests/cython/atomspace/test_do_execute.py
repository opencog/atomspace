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

    def test_do_execute(self):
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
