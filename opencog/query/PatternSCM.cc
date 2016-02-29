/*
 * PatternSCM.cc
 *
 * Guile Scheme bindings for the pattern matcher.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#include <opencog/atomutils/FuzzyMatchBasic.h>
#include <opencog/atomutils/TypeUtils.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeModule.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

#include "BindLinkAPI.h"
#include "PatternMatch.h"
#include "PatternSCM.h"

using namespace opencog;

// ========================================================
// Convenience wrapper
Handle PatternSCM::find_approximate_match(Handle hp)
{
	FuzzyMatchBasic fpm;
	RankedHandleSeq ranked_solns = fpm.perform_search(hp);
	HandleSeq solns;
	for (auto rs: ranked_solns)
		solns.emplace_back(rs.first);

	AtomSpace *as = SchemeSmob::ss_get_env_as("cog-fuzzy-match");
	return as->add_link(LIST_LINK, solns);
}

bool PatternSCM::value_is_type(Handle type, Handle val)
{
	return opencog::value_is_type(type, val);
}

bool PatternSCM::type_match(Handle left, Handle right)
{
	return opencog::type_match(left, right);
}

Handle PatternSCM::type_compose(Handle left, Handle right)
{
	return opencog::type_compose(left, right);
}

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
	// Run implication, assuming that the argument is a handle to an
	// BindLink containing variables, a pattern and a rewrite rules.
	_binders.push_back(new FunctionWrap(bindlink, "cog-bind", "query"));

	// Identical to bindlink above, except that it only returns the
	// first match.
	_binders.push_back(new FunctionWrap(single_bindlink,
	                   "cog-bind-single", "query"));

	// Identical to bindlink above, except that it only returns the
	// first N matches, assuming that N is the first argument and
	// the second is a BindLink handle.
	_binders.push_back(new FunctionWrap(first_n_bindlink,
	                   "cog-bind-first-n", "query"));

	// Attentional Focus function
	_binders.push_back(new FunctionWrap(af_bindlink,
	                   "cog-bind-af", "query"));

	// A bindlink that return a TV
	_binders.push_back(new FunctionWrap(satisfaction_link,
	                   "cog-satisfy", "query"));

	// Finds set of all variable groundings, assuming that the argument is
	// a handle to pattern.
	_binders.push_back(new FunctionWrap(satisfying_set,
	                   "cog-satisfying-set", "query"));

	// Identical to satisfying_set above, except it only returns the
	// first N matches, assuming that N is the first argument and
	// the second is a pattern handle.
	_binders.push_back(new FunctionWrap(first_n_satisfying_set,
	                   "cog-satisfying-set-first-n", "query"));

	// Rule recognition.
	_binders.push_back(new FunctionWrap(recognize,
	                   "cog-recognize", "query"));

	// Fuzzy matching. XXX FIXME. this is not technically
	// a query functon, and should probably be in some other
	// module, maybe some utilities module?
	define_scheme_primitive("cog-fuzzy-match",
		&PatternSCM::find_approximate_match, this, "query");

	// These below also belong somewhere else. Not sure where.
	define_scheme_primitive("cog-value-is-type?",
		&PatternSCM::value_is_type, this, "query");

	define_scheme_primitive("cog-type-match?",
		&PatternSCM::type_match, this, "query");

	define_scheme_primitive("cog-type-compose",
		&PatternSCM::type_compose, this, "query");
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
	patty.module_init();
}
