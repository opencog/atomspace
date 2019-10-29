/*
 * TypeUtilsSCM.cc
 *
 * Guile Scheme bindings for misc type utilities
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#ifdef HAVE_GUILE

#include <opencog/guile/SchemeModule.h>

namespace opencog {

class TypeUtilsSCM : public ModuleWrap
{
	protected:
		virtual void init(void);
		static std::vector<FunctionWrap*> _binders;

		// The three below belong in a different module...
		bool value_is_type(Handle, ValuePtr);
		bool type_match(Handle, ValuePtr);
		ValuePtr type_compose(Handle, ValuePtr);
	public:
		TypeUtilsSCM(void);
		~TypeUtilsSCM();
};

}

#include <opencog/atoms/core/TypeUtils.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

// ========================================================

bool TypeUtilsSCM::value_is_type(Handle type, ValuePtr val)
{
	return opencog::value_is_type(type, val);
}

bool TypeUtilsSCM::type_match(Handle left, ValuePtr right)
{
	return opencog::type_match(left, right);
}

ValuePtr TypeUtilsSCM::type_compose(Handle left, ValuePtr right)
{
	return opencog::type_compose(left, right);
}

// ========================================================

// XXX HACK ALERT This needs to be static, in order for python to
// work correctly.  The problem is that python keeps creating and
// destroying this class, but it expects things to stick around.
// Oh well. I guess that's OK, since the definition is meant to be
// for the lifetime of the process, anyway.
std::vector<FunctionWrap*> TypeUtilsSCM::_binders;

TypeUtilsSCM::TypeUtilsSCM(void) :
	ModuleWrap("opencog type-utils")
{}

/// This is called while (opencog type-utils) is the current module.
/// Thus, all the definitions below happen in that module.
void TypeUtilsSCM::init(void)
{
	// These below belong somewhere else. Not sure where.
	// Perhaps a deep-type module or type-reasoning module?
	// dependent-type module? We don't have dependent types, yet.
	define_scheme_primitive("cog-value-is-type?",
		&TypeUtilsSCM::value_is_type, this, "type-utils");

	define_scheme_primitive("cog-type-match?",
		&TypeUtilsSCM::type_match, this, "type-utils");

	define_scheme_primitive("cog-type-compose",
		&TypeUtilsSCM::type_compose, this, "type-utils");
}

TypeUtilsSCM::~TypeUtilsSCM()
{
#if PYTHON_BUG_IS_FIXED
	for (FunctionWrap* pw : _binders)
		delete pw;
#endif
}


extern "C" {
void opencog_type_utils_init(void);
};

void opencog_type_utils_init(void)
{
	static TypeUtilsSCM patty;
	patty.module_init();
}
#endif // HAVE_GUILE
