/*
 * chem-types-test.cc
 *
 * Unit test for custom atom types with custom short names.
 * Based on examples/type-system/apps/chemain.cc
 */

#include <stdio.h>
#include <string>
#include <cassert>

#include <opencog/atomspace/AtomSpace.h>

// These include files are in the build directory.
#include "tests/atoms/type-system/demo-types/chem_types.h"
#include "tests/atoms/type-system/demo-types/atom_names.h"

using namespace opencog;

int main(int argc, char* argv[])
{
	printf("Testing custom atom types with short names...\n");

	// Create the AtomSpace into which we'll place everything.
	AtomSpacePtr asp = createAtomSpace();

	// Test 1: Create atoms using short-name constructors
	printf("Test 1: Short-name constructors...\n");
	Handle methane = asp->add_atom(
		Molecule(
			SB(C("carbon"), H("proton 1")),
			SB(C("carbon"), H("proton 2")),
			SB(C("carbon"), H("proton 3")),
			SB(C("carbon"), H("proton 4"))));

	assert(methane != nullptr);
	printf("  Created methane molecule: OK\n");

	// Verify the types are correct
	Handle carbon = asp->add_atom(C("carbon"));
	assert(carbon->get_type() == CARBON_NODE);
	printf("  Carbon type check: OK\n");

	Handle hydrogen = asp->add_atom(H("proton 1"));
	assert(hydrogen->get_type() == HYDROGEN_NODE);
	printf("  Hydrogen type check: OK\n");

	// Test 2: Create atoms using long-form type names
	printf("Test 2: Long-form type constructors...\n");
	Handle water = asp->add_atom(
		createLink(MOLECULE,
			createLink(SINGLE_BOND,
				createNode(OXYGEN_NODE, "oxy"),
				createNode(HYDROGEN_NODE, "h1")),
			createLink(SINGLE_BOND,
				createNode(OXYGEN_NODE, "oxy"),
				createNode(HYDROGEN_NODE, "h2"))));

	assert(water != nullptr);
	printf("  Created water molecule: OK\n");

	// Test 3: Verify type constants are usable
	printf("Test 3: Type constants...\n");
	Type ta = CARBON_NODE;
	Type tb = NITROGEN_NODE;
	std::string sa = "carbo";
	std::string sb = "nitro";

	Handle cyanide = asp->add_atom(
		createLink(MOLECULE,
			createLink(TRIPLE_BOND,
				createNode(ta, sa),
				createNode(tb, sb))));

	assert(cyanide != nullptr);
	printf("  Created cyanide using type constants: OK\n");

	// Test 4: Verify custom types without short names work
	printf("Test 4: Types without custom short names...\n");
	Handle isotope = asp->add_atom(createNode(ISOTOPE_NODE, "some isotope"));
	assert(isotope != nullptr);
	assert(isotope->get_type() == ISOTOPE_NODE);
	printf("  IsotopeNode type check: OK\n");

	// Test 5: Verify all bond types work with short names
	printf("Test 5: All bond types (short names)...\n");
	Handle sb_bond = asp->add_atom(SB(C("c1"), C("c2")));
	Handle db_bond = asp->add_atom(DB(C("c3"), C("c4")));
	Handle tb_bond = asp->add_atom(TB(C("c5"), N("n1")));
	Handle ab_bond = asp->add_atom(AB(C("c6"), C("c7")));

	assert(sb_bond->get_type() == SINGLE_BOND);
	assert(db_bond->get_type() == DOUBLE_BOND);
	assert(tb_bond->get_type() == TRIPLE_BOND);
	assert(ab_bond->get_type() == AROMATIC_BOND);
	printf("  All bond types (short names): OK\n");

	// Test 6: Long names (CamelCase) also work
	printf("Test 6: Long CamelCase names...\n");
	Handle sb_long = asp->add_atom(SingleBond(Carbon("lc1"), Hydrogen("lh1")));
	Handle db_long = asp->add_atom(DoubleBond(Carbon("lc2"), Oxygen("lo1")));
	Handle tb_long = asp->add_atom(TripleBond(Carbon("lc3"), Nitrogen("ln1")));

	assert(sb_long->get_type() == SINGLE_BOND);
	assert(db_long->get_type() == DOUBLE_BOND);
	assert(tb_long->get_type() == TRIPLE_BOND);
	printf("  Long CamelCase names: OK\n");

	printf("\nAll tests passed!\n");
	return 0;
}
