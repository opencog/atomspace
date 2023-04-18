/*
 * ExampleSCM.cc
 *
 * Example Guile Scheme bindings for C++ code.
 */

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>

#include "ExampleSCM.h"

// ========================================================

using namespace opencog;

// These functions will be wrapped in the guile module.
// An example of wrapping methods is given in PrimitiveExample.cc.
//
Handle ss_print(const Handle& h)
{
	printf("Hello! I got this atom: %s\n", h->to_short_string().c_str());
	return h;
}

TruthValuePtr ss_printmore(const Handle& h)
{
	printf("This is the atom: %s\n", h->to_string().c_str());
	return h->getTruthValue();
}

// ========================================================

// ModuleWrap takes the string name of the guile module.
ExampleSCM::ExampleSCM(void) :
	ModuleWrap("opencog example")
{}

/// This is called while (opencog example) is the current module.
/// Thus, all the definitions below happen in that module.
void ExampleSCM::init(void)
{
	define_scheme_primitive("hey-print", ss_print, "example");
	define_scheme_primitive("hey-printmore", ss_printmore, "example");
}

// Create a single static instance.
void opencog_example_init(void)
{
	static ExampleSCM example;
	example.module_init();
}
