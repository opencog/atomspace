#!/usr/bin/env python3
#
# chem-types-test.py
#
# Unit test for custom atom types with custom short names in Python.
# Based on examples/type-system/apps/chemy_hello.py
#

import unittest
import sys

# Import the AtomSpace, and the basic AtomSpace types
from opencog.atomspace import AtomSpace
from opencog.atomspace import set_thread_atomspace

# Import all of the chemical element types, and bond types too.
from test_chempydemo import *


class ChemTypesTest(unittest.TestCase):

    def setUp(self):
        self.spa = AtomSpace()
        set_thread_atomspace(self.spa)

    def tearDown(self):
        pass

    def test_create_atoms_with_short_names(self):
        """Test creating atoms using short-name constructors"""
        # Create a Magnesium atom
        mg = Mg('foo')
        self.assertIsNotNone(mg)
        # Custom-named types use their short name as TYPE_NAME
        self.assertEqual(mg.type_name, 'Mg')

    def test_create_bonds(self):
        """Test creating bonds with short-name constructors"""
        ch_bond = SB(C('carbon'), H('hydrogen'))
        self.assertIsNotNone(ch_bond)
        # Custom-named types use their short name as TYPE_NAME
        self.assertEqual(ch_bond.type_name, 'SB')

    def test_create_molecule(self):
        """Test creating a molecule"""
        methane = Molecule(
            SB(C('1'), H('1')),
            SB(C('1'), H('2')),
            SB(C('1'), H('3')),
            SB(C('1'), H('4')))
        self.assertIsNotNone(methane)
        self.assertEqual(methane.type_name, 'Molecule')

    def test_element_types(self):
        """Test various element types"""
        hydrogen = H('h1')
        carbon = C('c1')
        nitrogen = N('n1')
        oxygen = O('o1')

        # Custom-named types use their short name as TYPE_NAME
        self.assertEqual(hydrogen.type_name, 'H')
        self.assertEqual(carbon.type_name, 'C')
        self.assertEqual(nitrogen.type_name, 'N')
        self.assertEqual(oxygen.type_name, 'O')

    def test_bond_types(self):
        """Test all bond types"""
        single = SB(C('c1'), C('c2'))
        double = DB(C('c3'), C('c4'))
        triple = TB(C('c5'), N('n1'))
        aromatic = AB(C('c6'), C('c7'))

        # Custom-named types use their short name as TYPE_NAME
        self.assertEqual(single.type_name, 'SB')
        self.assertEqual(double.type_name, 'DB')
        self.assertEqual(triple.type_name, 'TB')
        self.assertEqual(aromatic.type_name, 'AB')

    def test_carbon14_execution(self):
        """Test Carbon14 atom execution"""
        c14 = Carbon14Node('found in wood')
        self.assertIsNotNone(c14)
        self.assertEqual(c14.type_name, 'Carbon14Node')

        decay_products = c14.execute()
        self.assertIsNotNone(decay_products)

        # Check that decay products contain expected types
        products_list = decay_products.to_list()
        self.assertTrue(len(products_list) > 0)

        # First product should be Nitrogen14Node
        nitrogen = products_list[0]
        self.assertEqual(nitrogen.type_name, 'Nitrogen14Node')


if __name__ == '__main__':
    unittest.main()
