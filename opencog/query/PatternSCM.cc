/*
 * PatternSCM.cc
 *
 * Guile Scheme bindings for the pattern matcher.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#ifdef HAVE_GUILE

#include <opencog/guile/SchemeModule.h>
#include <opencog/atoms/pattern/PatternLink.h>

namespace opencog {

class PatternSCM : public ModuleWrap
{
	protected:
		virtual void init(void);
		static std::vector<FunctionWrap*> _binders;
		Handle find_approximate_match(Handle);
		bool value_is_type(Handle, Handle);
		bool type_match(Handle, Handle);
		Handle type_compose(Handle, Handle);
	public:
		PatternSCM(void);
		~PatternSCM();
};

}

#include <opencog/atomutils/FuzzyMatchBasic.h>
#include <opencog/atomutils/TypeUtils.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

#include "BindLinkAPI.h"
#include "PatternMatch.h"

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
// for the lifetime of the process, anyway.
std::vector<FunctionWrap*> PatternSCM::_binders;

PatternSCM::PatternSCM(void) :
	ModuleWrap("opencog query")
{}

static TruthValuePtr do_satlink(AtomSpace* as, const Handle& hlink)
{
	Handle plp(hlink);
	// If not already a PatternLink, then WRAP it in a PattrnLink.
	if (not nameserver().isA(hlink->get_type(), PATTERN_LINK))
		plp = createPatternLink(hlink);
	return satisfaction_link(as, plp);
}

/// This is called while (opencog query) is the current module.
/// Thus, all the definitions below happen in that module.
void PatternSCM::init(void)
{
	// Run implication, assuming that the first argument is a handle to a
	// BindLink containing variables, a pattern and a rewrite rules.
	// Returns the first N matches, assuming that N is the second argument.
	_binders.push_back(new FunctionWrap(bindlink,
	                   "cog-bind-first-n", "query"));

	// A bindlink that returns a TV
	_binders.push_back(new FunctionWrap(do_satlink,
	                   "cog-satisfy", "query"));

	// Finds set of all variable groundings, assuming that the first
	// argument is a handle to pattern. Returns the first N matches,
	// assuming that N is the second argument.
	_binders.push_back(new FunctionWrap(satisfying_set,
	                   "cog-satisfying-set-first-n", "query"));

	// Rule recognition.
	_binders.push_back(new FunctionWrap(recognize,
	                   "cog-recognize", "query"));

	// Fuzzy matching. XXX FIXME. This is not technically
	// a query functon, and should probably be in some other
	// module, maybe some utilities module?
	define_scheme_primitive("cog-fuzzy-match",
		&PatternSCM::find_approximate_match, this, "query");

	// These below also belong somewhere else. Not sure where.
	// Perhaps a deep-type module or type-reasoning module?
	// dependent-type module? We don't have dependent types, yet.
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


extern "C" {
void opencog_query_init(void);
};

void opencog_query_init(void)
{
	static PatternSCM patty;
	patty.module_init();
}
#endif // HAVE_GUILE
