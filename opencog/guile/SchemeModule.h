/*
 * SchemeModule.h
 *
 * Simplified wrapper for creating guile modules.
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#ifndef _OPENCOG_SCHEME_MODULE_H
#define _OPENCOG_SCHEME_MODULE_H

#include <opencog/atomspace/Handle.h>
#include <opencog/truthvalue/TruthValue.h>
#include <opencog/atomspace/atom_types.h>

namespace opencog {

class AtomSpace;

/// Wrapper class, to invoke misc extension code from guile.
class FunctionWrap
{
	private:
		void (*_func_v_s)(const std::string&);
		const std::string& (*_func_s)();
		void (*_func_v_b)(bool);
		Handle (*_func_h_ah)(AtomSpace*, const Handle&);
		Handle (*_func_h_ahhh)(AtomSpace*, const Handle&,
		                      const Handle&, const Handle&);
		Handle (*_func_h_ahtq)(AtomSpace*, const Handle&, Type, const HandleSeq&);
		Handle (*_func_h_ahtqb)(AtomSpace*, const Handle&, Type,
                                const HandleSeq&, bool);
		HandleSeq (*_func_q_ah)(AtomSpace*, const Handle&);

		// Wrappers are used because define_scheme_primitive expect a
		// class function member pointer as opposed to a dangling
		// function pointer.
		void wrapper_v_s(const std::string&);
		const std::string& wrapper_s();
		void wrapper_v_b(bool);

		// These wrappers abstract the atomspace away.
		Handle as_wrapper_h_h(Handle);
		Handle as_wrapper_h_hhh(Handle, Handle, Handle);
		Handle as_wrapper_h_htq(Handle, Type, const HandleSeq&);
		Handle as_wrapper_h_htqb(Handle, Type, const HandleSeq&, bool);

		// These wrappers return a TruthValuePtr and abstract the
		// atomspace away.
		TruthValuePtr (*_pred_ah)(AtomSpace*, const Handle&);
		TruthValuePtr as_wrapper_p_h(Handle);

		// This wrapper return HandleSeq and abstract the atomspace away.
		HandleSeq as_wrapper_q_h(Handle);

		const char *_name;  // scheme name of the c++ function.
	public:
		FunctionWrap(void (*)(bool),
		             const char*, const char*);
		FunctionWrap(const std::string& (*)(),
		             const char*, const char*);
		FunctionWrap(void (*)(const std::string&),
		             const char*, const char*);
		FunctionWrap(Handle (*)(AtomSpace*, const Handle&),
		             const char*, const char*);
		FunctionWrap(Handle (*)(AtomSpace*, const Handle&,
		                        Type, const HandleSeq&),
		             const char*, const char*);
		FunctionWrap(Handle (*)(AtomSpace*, const Handle&,
		                        Type, const HandleSeq&, bool),
		             const char*, const char*);
		FunctionWrap(Handle (*)(AtomSpace*, const Handle&, const Handle&,
		                        const Handle&),
		             const char*, const char*);
		FunctionWrap(TruthValuePtr (*)(AtomSpace*, const Handle&),
		             const char*, const char*);
		FunctionWrap(HandleSeq (*)(AtomSpace*, const Handle&),
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
