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
	: _func_h_ah(f), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_h_h, this, modname);
}

FunctionWrap::FunctionWrap(Handle (f)(AtomSpace*, const Handle&, size_t),
                           const char* funcname, const char* modname)
	: _func_h_ahz(f), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_h_hz, this, modname);
}

FunctionWrap::FunctionWrap(TruthValuePtr (p)(AtomSpace*, const Handle&),
                           const char* funcname, const char* modname)
	: _pred_ah(p), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_p_h, this, modname);
}

Handle FunctionWrap::as_wrapper_h_h(Handle h)
{
	// XXX we should also allow opt-args to be a list of handles
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	return _func_h_ah(as, h);
}

Handle FunctionWrap::as_wrapper_h_hz(Handle h, size_t sz)
{
	// XXX we should also allow opt-args to be a list of handles
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	return _func_h_ahz(as, h, sz);
}

TruthValuePtr FunctionWrap::as_wrapper_p_h(Handle h)
{
	// XXX we should also allow opt-args to be a list of handles
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	return _pred_ah(as, h);
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
