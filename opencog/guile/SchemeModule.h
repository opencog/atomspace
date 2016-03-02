/*
 * SchemeModule.h
 *
 * Simplified wrapper for creating guile modules.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#ifndef _OPENCOG_SCHEME_MODULE_H
#define _OPENCOG_SCHEME_MODULE_H

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/atom_types.h>
#include <opencog/truthvalue/TruthValue.h>

namespace opencog {

class AtomSpace;

/// Wrapper class, to invoke misc extension code from guile.
class FunctionWrap
{
	private:
		// Wrappers are used because define_scheme_primitive expects
		// a class function member pointer as opposed to a dangling
		// function pointer.

		// These wrappers abstract the atomspace away.
		Handle (*_func_h_ah)(AtomSpace*, const Handle&);
		Handle (*_func_h_ahz)(AtomSpace*, const Handle&, size_t);
		Handle as_wrapper_h_h(Handle);
		Handle as_wrapper_h_hz(Handle, size_t);

		// These wrappers return a TruthValuePtr and abstract the
		// atomspace away.
		TruthValuePtr (*_pred_ah)(AtomSpace*, const Handle&);
		TruthValuePtr as_wrapper_p_h(Handle);

		const char *_name;  // scheme name of the c++ function.
	public:
		FunctionWrap(Handle (*)(AtomSpace*, const Handle&),
		             const char*, const char*);
		FunctionWrap(Handle (*)(AtomSpace*, const Handle&, size_t),
		             const char*, const char*);
		FunctionWrap(TruthValuePtr (*)(AtomSpace*, const Handle&),
		             const char*, const char*);
};

class ModuleWrap
{
	private:
		static void* init_in_guile(void*);
		static void init_in_module(void*);
		const char* _modname;
	protected:
		virtual void init(void) = 0;
	public:
		ModuleWrap(const char*);
		void module_init(void);
		virtual ~ModuleWrap();
};

}

#endif // _OPENCOG_SCHEME_MODULE_H
