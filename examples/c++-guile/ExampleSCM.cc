/*
 * ExampleSCM.cc
 *
 * Example Guile Scheme bindings for C++ code.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */


#include <cstddef>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeModule.h>

#include "ExampleSCM.h"


// ========================================================

using namespace opencog;

// Below are the functions to be placed into the module.
// At this time, the way that the module wrapper is designed,
// the wrapper only allows two types of functions to be wrapped:
// Those that take a single handle, and return either a handle,
// or a truth value.  The SchemePrimitive code allows far more
// general functions to be wrapped; however, for the simplified
// creation of modules, these two types seems to be enough to
// handle all current cases.
//
// You can wrap more general methods; see the PrimitiveExample.cc
// for that.
static Handle ss_print(AtomSpace* atomspace, const Handle& h)
{
	Handle reth = h;
	printf("Hello I got this atom: %s\n", h->to_short_string().c_str());
	return reth;
}

static TruthValuePtr ss_printmore(AtomSpace* atomspace, const Handle& h)
{
	printf("This is the atom: %s\n", h->to_string().c_str());
	return h->getTruthValue();
}

// ========================================================

// XXX HACK ALERT This needs to be static, in order for python to
// work correctly.  The problem is that python keeps creating and
// destroying this class, but it expects things to stick around.
// Oh well. I guess that's OK, since the definition is meant to be
// for the lifetime of the server, anyway.
std::vector<FunctionWrap*> ExampleSCM::_binders;

// ModuleWrap takes the string name of the guile module.
ExampleSCM::ExampleSCM(void) :
	ModuleWrap("opencog example")
{}

/// This is called while (opencog example) is the current module.
/// Thus, all the definitions below happen in that module.
void ExampleSCM::init(void)
{
	_binders.push_back(new FunctionWrap(ss_print,
	                    "hey-print", "example"));

	_binders.push_back(new FunctionWrap(ss_printmore,
	                   "hey-printmore", "example"));
}

ExampleSCM::~ExampleSCM()
{
#if PYTHON_BUG_IS_FIXED
	for (FunctionWrap* pw : _binders)
		delete pw;
#endif
}

// Create a single static instance.
void opencog_example_init(void)
{
	static ExampleSCM example;
	example.module_init();
}
