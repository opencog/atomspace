/*
 * SchemeModule.h
 *
 * Simplified wrapper for creating guile modules.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#ifndef _OPENCOG_SCHEME_MODULE_H
#define _OPENCOG_SCHEME_MODULE_H

#include <opencog/atomspace/Handle.h>
#include <opencog/atomspace/TruthValue.h>

namespace opencog {

class AtomSpace;

/// Wrapper class, to invoke misc extension code from guile.
class FunctionWrap
{
	private:
		Handle (*_func)(AtomSpace*, const Handle&);
		Handle wrapper(Handle);

		TruthValuePtr (*_pred)(AtomSpace*, const Handle&);
		TruthValuePtr prapper(Handle);

		const char *_name;  // scheme name of the c++ function.
	public:
		FunctionWrap(Handle (*)(AtomSpace*, const Handle&),
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
		virtual ~ModuleWrap();
};

}

#endif // _OPENCOG_SCHEME_MODULE_H

