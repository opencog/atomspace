/*
 * chemain.cc
 *
 * Example C++ demo making use of the new demo Atom Types.
 */

#include <stdio.h>
#include <string>

#include <opencog/atomspace/AtomSpace.h>

// These include files are in the build directory.
// They are not being installed for this demo.
#include "examples/type-system/demo-types/chem_types.h"
#include "examples/type-system/demo-types/atom_names.h"

using namespace opencog;

int main(int argc, char* argv[])
{
	printf("Hello world from %s\n", argv[0]);

	// Create the AtomSpace into which we'll place everything.
	AtomSpacePtr asp = createAtomSpace();

	// Insert some atoms.
	Handle methane = asp->add_atom(
		Molecule(
			SB(C("carbon"), H("proton 1")),
			SB(C("carbon"), H("proton 2")),
			SB(C("carbon"), H("proton 3")),
			SB(C("carbon"), H("proton 4"))));

	printf("Methane looks like this:\n%s\n\n",
		methane->to_short_string().c_str());

	// Atoms can also be created indirectly, from thier types.
	// This form is more verbose, but avoids using named functions,
	// and instead uses only two functions: createNode and createLink.
	Handle water = asp->add_atom(
		createLink(MOLECULE,
			createLink(SINGLE_BOND,
				createNode(OXYGEN_NODE, "oxy"),
				createNode(HYDROGEN_NODE, "h1")),
			createLink(SINGLE_BOND,
				createNode(OXYGEN_NODE, "oxy"),
				createNode(HYDROGEN_NODE, "h2"))));

	printf("Water looks like this:\n%s\n\n",
		water->to_short_string().c_str());

	// The types can be passed as arguments to functions.
	Type ta = CARBON_NODE;
	Type tb = NITROGEN_NODE;
	std::string sa = "carbo";
	std::string sb = "nitro";

	Handle cyanide = asp->add_atom(
		createLink(MOLECULE,
			createLink(TRIPLE_BOND,
				createNode(ta, sa),
				createNode(tb, sb))));

	printf("Cyanide looks like this:\n%s\n\n",
		cyanide->to_short_string().c_str());
}
