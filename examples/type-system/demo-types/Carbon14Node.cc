/*
 * Carbon14Node.cc
 *
 * Example of an an "active" Atom that can "do something" when
 * it is executed.
 */

#include <vector>

#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atomspace/AtomSpace.h>

#include "Carbon14Node.h"

using namespace opencog;

// This method is called whenever this Atom Type needs to be executed.
// There are many, many ways to trigger atom execution. The return
// value must be a Value. Note that Atoms are a kind of Value.  If
// there's no return, just use VoidValue.
//
// Values, unlike Atoms, are NOT held in an AtomSpace. However, if
// this function needs to create Atoms, they should be placed in the
// `as` frame. The AtomSpace argument is a "frame", it might be the
// main atomspace, or it might be a temporary space, or something more
// general, depending on the situation. (AtomSpaces can be arranged into
// so-called 'Kripke frames'.)
//
// The `silent` boolean flag is used to indicate how exceptions should
// be thrown. Silent exceptions are usually `SyntaxException`s, and the
// user will never see these. The non-silent exceptions will be
// presented to the user as a long, ugly error emssage and stack trace.
// This demo does not throw any exceptions.
ValuePtr Carbon14Node::execute(AtomSpace* as, bool silent)
{
	printf("I am a decaying carbon-14 atom of '%s' origin\n", kind.c_str());

	ValueSeq decay_products;
	decay_products.push_back(
		as->add_node(NITROGEN14_NODE, std::string(get_name())));
	decay_products.push_back(
		as->add_node(ELECTRON_NODE, "carbon-14 decay electron"));
	decay_products.push_back(
		as->add_node(ANTINEUTRINO_NODE, "carbon-14 decay antineutrino"));

	// The released energy in keV
	decay_products.push_back(createFloatValue(156.5));
	decay_products.push_back(createStringValue("keV"));

	// FYI Float Values are actually vectors
	std::vector<double> pi({3,1,4,1,5,9,2,6,5,3});
	decay_products.push_back(createFloatValue(pi));

	// String values are also vectors.
	std::vector<std::string> labels({"decimal", "digits", "of", "pi"});
	decay_products.push_back(createStringValue(labels));

	ValuePtr vp = createLinkValue(decay_products);
	return vp;
}

// Boilerplate.  Leave this alone.
DEFINE_NODE_FACTORY(Carbon14Node, CARBON14_NODE)
