/*
 * PatternSCM.cc
 *
 * Guile Scheme bindings for the pattern matcher.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeModule.h>

#include "BindLinkAPI.h"
#include "PatternMatch.h"
#include "PatternSCM.h"
#include "FuzzyMatch/FuzzyPatternMatch.h"


using namespace opencog;

// ========================================================

// XXX HACK ALERT This needs to be static, in order for python to
// work correctly.  The problem is that python keeps creating and
// destroying this class, but it expects things to stick around.
// Oh well. I guess that's OK, since the definition is meant to be
// for the lifetime of the server, anyway.
std::vector<FunctionWrap*> PatternSCM::_binders;

PatternSCM::PatternSCM(void) :
	ModuleWrap("opencog query")
{}

/// This is called while (opencog query) is the current module.
/// Thus, all the definitions below happen in that module.
void PatternSCM::init(void)
{
	// Run implication, assuming that the argument is a handle to
	// an BindLink containing variables and an ImplicationLink.
	_binders.push_back(new FunctionWrap(bindlink, "cog-bind", "query"));

	// Identical to do_bindlink above, except that it only returns the
	// first match.
	_binders.push_back(new FunctionWrap(single_bindlink,
	                   "cog-bind-single", "query"));

	// Mystery function
	_binders.push_back(new FunctionWrap(pln_bindlink,
	                   "cog-bind-pln", "query"));

   // Fuzzy matching.
	_binders.push_back(new FunctionWrap(find_approximate_match,
	                   "cog-fuzzy-match", "query"));

	// A bindlink that return a TV
	_binders.push_back(new FunctionWrap(satisfaction_link,
	                   "cog-satisfy", "query"));

	_binders.push_back(new FunctionWrap(satisfying_set,
	                   "cog-satisfying-set", "query"));
}

PatternSCM::~PatternSCM()
{
#if PYTHON_BUG_IS_FIXED
	for (FunctionWrap* pw : _binders)
		delete pw;
#endif
}


void opencog_query_init(void)
{
	static PatternSCM patty;
}
