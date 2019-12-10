//
// examples/c++/basic.cc
//
// A basic example of creating Atoms in an AtomSpace.

#include <opencog/atoms/atom_types/atom_names.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

int main()
{
	AtomSpace* as = new AtomSpace();

	as->add_atom(ConceptNode("foobar"));
	printf("So far its this: %s\n", as->to_string().c_str());

	as->add_atom(
		EvaluationLink(
			PredicateNode("bling"),
			ListLink(
				ConceptNode("foo"),
				ConceptNode("bar"))));

	printf("Now it is this: %s\n", as->to_string().c_str());
}
