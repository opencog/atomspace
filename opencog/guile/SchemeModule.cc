/*
 * SchemeModule.cc
 *
 * Simplified API for creating guile modules.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemeModule.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

FunctionWrap::FunctionWrap(Handle (f)(AtomSpace*, const Handle&),
                           const char* funcname, const char* modname)
	: _func(f), _pred(NULL), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::wrapper, this, modname);
}

FunctionWrap::FunctionWrap(Handle (f)(AtomSpace*, const Handle&, Type, const HandleSeq&),
			   const char* funcname, const char* modname)
	: _func_htq(f), _pred(NULL), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::wrapper_htq, this, modname);
}

FunctionWrap::FunctionWrap(TruthValuePtr (p)(AtomSpace*, const Handle&),
                           const char* funcname, const char* modname)
	: _func(NULL), _pred(p), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::prapper, this, modname);
}

Handle FunctionWrap::wrapper(Handle h)
{
	// XXX we should also allow opt-args to be a list of handles
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	return _func(as, h);
}

Handle FunctionWrap::wrapper_htq(Handle h, Type t, const HandleSeq& seq)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	return _func_htq(as, h, t, seq);
}

TruthValuePtr FunctionWrap::prapper(Handle h)
{
	// XXX we should also allow opt-args to be a list of handles
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	return _pred(as, h);
}

// ========================================================

ModuleWrap::ModuleWrap(const char* m) :
	_modname(m)
{}

void ModuleWrap::module_init(void)
{
	scm_with_guile(init_in_guile, this);
}

void* ModuleWrap::init_in_guile(void* data)
{
	// init_in_module(NULL);
	ModuleWrap* self = (ModuleWrap*) data;

	scm_c_define_module(self->_modname, init_in_module, data);
	scm_c_use_module(self->_modname);

	return NULL;
}

/// This is called while _modname is the current module.
/// Thus, all the definitions below happen in that module.
void ModuleWrap::init_in_module(void* data)
{
	ModuleWrap* self = (ModuleWrap*) data;
	self->init();
}

ModuleWrap::~ModuleWrap()
{
}
