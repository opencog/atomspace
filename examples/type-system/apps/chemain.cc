/*
 * chemain.cc
 *
 * Example C++ demo making use of the new demo Atom Types.
 */

#include <stdio.h>

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
	Handle methane = asp->add(
		Na("foo"));

	printf("Methane looks like this: %s\n",
		methane->to_short_string().c_str());
}
