//
// examples/c++/basic.cc
//
// A basic example of creating Atoms in an AtomSpace.

#include <opencog/atoms/atom_types/atom_names.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

int main()
{
	// Create a new AtomSpace.
	AtomSpacePtr as = createAtomSpace();

	// Create a ConceptNode Atom, place it in the AtomSpace.
	Handle h = as->add_atom(Concept("foobar"));

	// Print it, take a look:
	printf("The new Atom is %s\n\n", h->to_short_string().c_str());

	// A more verbose print:
	printf("The new Atom and its hash: %s\n\n", h->to_string().c_str());

	// Print the atomspace contents.
	// Note that the unique Atom ID's (64-bit hashes) are printed.
	printf("The AtomSpace contains this:\n%s\n", as->to_string().c_str());

	// Create an EvaluationLink Atom, place it in the AtomSpace.
	as->add_atom(
		Evaluation(
			Predicate("bling"),
			List(
				Concept("foo"),
				Concept("bar"))));

	printf("Now the AtomSpace contains this:\n%s\n", as->to_string().c_str());
}
