/*
 * SchemeModule.cc
 *
 * Guile Scheme bindings for the pattern matcher.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

#include "BindLinkAPI.h"
#include "PatternMatch.h"
#include "ModuleWrap.h"
#include "FuzzyMatch/FuzzyPatternMatch.h"


using namespace opencog;

FunctionWrap::FunctionWrap(Handle (f)(AtomSpace*, const Handle&), const char* n)
	: _func(f), _pred(NULL), _name(n)
{
#ifdef HAVE_GUILE
	define_scheme_primitive(_name, &FunctionWrap::wrapper, this, "query");
#endif
}

FunctionWrap::FunctionWrap(TruthValuePtr (p)(AtomSpace*, const Handle&), const char* n)
	: _func(NULL), _pred(p), _name(n)
{
#ifdef HAVE_GUILE
	define_scheme_primitive(_name, &FunctionWrap::prapper, this, "query");
#endif
}

Handle FunctionWrap::wrapper(Handle h)
{
#ifdef HAVE_GUILE
	// XXX we should also allow opt-args to be a list of handles
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	return _func(as, h);
#else
	return Handle::UNDEFINED;
#endif
}

TruthValuePtr FunctionWrap::prapper(Handle h)
{
#ifdef HAVE_GUILE
	// XXX we should also allow opt-args to be a list of handles
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	return _pred(as, h);
#else
	return TruthValuePtr();
#endif
}

// ========================================================

// XXX HACK ALERT This needs to be static, in order for python to
// work correctly.  The problem is that python keeps creating and
// destroying this class, but it expects things to stick around.
// Oh well. I guess that's OK, since the definition is meant to be
// for the lifetime of the server, anyway.
std::vector<FunctionWrap*> ModuleWrap::_binders;

ModuleWrap::ModuleWrap(void)
{
	static bool is_init = false;
	if (is_init) return;
	is_init = true;
	scm_with_guile(init_in_guile, NULL);
}

void* ModuleWrap::init_in_guile(void*)
{
	// init_in_module(NULL);
	scm_c_define_module("opencog query", init_in_module, NULL);
	scm_c_use_module("opencog query");

	return NULL;
}

/// This is called while (opencog query) is the current module.
/// Thus, all the definitions below happen in that module.
void ModuleWrap::init_in_module(void*)
{
	// Run implication, assuming that the argument is a handle to
	// an BindLink containing variables and an ImplicationLink.
	_binders.push_back(new FunctionWrap(bindlink, "cog-bind"));

	// Identical to do_bindlink above, except that it only returns the
	// first match.
	_binders.push_back(new FunctionWrap(single_bindlink, "cog-bind-single"));

	// Mystery function
	_binders.push_back(new FunctionWrap(pln_bindlink, "cog-bind-pln"));

   // Fuzzy matching.
	_binders.push_back(new FunctionWrap(find_approximate_match, "cog-fuzzy-match"));

	// A bindlink that return a TV
	_binders.push_back(new FunctionWrap(satisfaction_link, "cog-satisfy"));

	_binders.push_back(new FunctionWrap(satisfying_set, "cog-satisfying-set"));
}

ModuleWrap::~ModuleWrap()
{
#if PYTHON_BUG_IS_FIXED
	for (FunctionWrap* pw : _binders)
		delete pw;
#endif
}


void opencog_query_init(void)
{
	static ModuleWrap patty;
}
