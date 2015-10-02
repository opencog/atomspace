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

FunctionWrap::FunctionWrap(void (f)(const std::string&),
                           const char* funcname, const char* modname)
	: _func_v_s(f), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::wrapper_v_s, this, modname);
}

FunctionWrap::FunctionWrap(void (f)(bool),
                           const char* funcname, const char* modname)
	: _func_v_b(f), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::wrapper_v_b, this, modname);
}

FunctionWrap::FunctionWrap(const std::string& (f)(),
                           const char* funcname, const char* modname)
	: _func_s(f), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::wrapper_s, this, modname);
}

FunctionWrap::FunctionWrap(Handle (f)(AtomSpace*, const Handle&),
                           const char* funcname, const char* modname)
	: _func_h_ah(f), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_h_h, this, modname);
}

FunctionWrap::FunctionWrap(Handle (f)(AtomSpace*, const Handle&,
                                      const Handle&, const Handle&),
			   const char* funcname, const char* modname)
	: _func_h_ahhh(f), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_h_hhh, this, modname);
}

FunctionWrap::FunctionWrap(Handle (f)(AtomSpace*, const Handle&, Type, const HandleSeq&),
			   const char* funcname, const char* modname)
	: _func_h_ahtq(f), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_wrapper_h_htq, this, modname);
}

FunctionWrap::FunctionWrap(TruthValuePtr (p)(AtomSpace*, const Handle&),
                           const char* funcname, const char* modname)
	: _pred_ah(p), _name(funcname)
{
	define_scheme_primitive(_name, &FunctionWrap::as_prapper_h, this, modname);
}

void FunctionWrap::wrapper_v_s(const std::string& s)
{
	return _func_v_s(s);
}

const std::string& FunctionWrap::wrapper_s()
{
	return _func_s();
}

void FunctionWrap::wrapper_v_b(bool b)
{
	return _func_v_b(b);
}

Handle FunctionWrap::as_wrapper_h_h(Handle h)
{
	// XXX we should also allow opt-args to be a list of handles
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	return _func_h_ah(as, h);
}

Handle FunctionWrap::as_wrapper_h_hhh(Handle h1, Handle h2, Handle h3)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	return _func_h_ahhh(as, h1, h2, h3);
}

Handle FunctionWrap::as_wrapper_h_htq(Handle h, Type t, const HandleSeq& seq)
{
	AtomSpace *as = SchemeSmob::ss_get_env_as(_name);
	return _func_h_ahtq(as, h, t, seq);
}

TruthValuePtr FunctionWrap::as_prapper_h(Handle h)
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
