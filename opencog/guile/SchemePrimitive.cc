/*
 * SchemePrimitive.cc
 *
 * Allow C++ code to be invoked from scheme --
 * by defining a scheme primitive function.
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <exception>

#include "SchemeEval.h"
#include "SchemePrimitive.h"
#include "SchemeSmob.h"

using namespace opencog;

bool PrimitiveEnviron::is_inited = false;

#if defined(HAVE_GUILE2) || defined(HAVE_GUILE3)
 #define C(X) ((scm_t_subr) X)
#else
 #define C(X) ((SCM (*) ()) X)
#endif


/**
 * initialization code -- This is currently called under a
 * lock, from SchemeEval::init()
 */
void PrimitiveEnviron::init(void)
{
	if (is_inited) return;
	is_inited = true;
	scm_c_define_module("opencog extension", init_in_module, NULL);

	// Use, immediately after define. Kind of hacky-ish, but assorted
	// C++ code will fail if they forget to use: so we use it, for them.
	scm_c_use_module("opencog extension");
}

/// This is called while (opencog extension) is the current module.
/// Thus, opencog-extension is only defined in that module.
void PrimitiveEnviron::init_in_module(void*)
{
	scm_c_define_gsubr("opencog-extension", 2,0,0, C(do_call));

	// Export, because other modules will need to call this directly.
	scm_c_export("opencog-extension", NULL);
}

PrimitiveEnviron::~PrimitiveEnviron() {}

void PrimitiveEnviron::do_register(const char * module,
                                   const char *name,
                                   int nargs,
                                   bool xport)
{
	// Now enter guile mode, and do the actual work there.
	tmp_module = module;
	tmp_name = name;
	tmp_nargs = nargs;
	tmp_export = xport;
	scm_with_guile(c_wrap_register, this);
}

void *PrimitiveEnviron::c_wrap_register(void *p)
{
	PrimitiveEnviron *self = (PrimitiveEnviron *) p;
	self->really_do_register(self->tmp_module, self->tmp_name,
	                         self->tmp_nargs, self->tmp_export);
	return NULL;
}

/**
 * Create a new smob that will store a pointer to "this", which, in
 * turn, holds a pointer to the C++ instance and the C++ method to be
 * invoked, when its called from scheme.  The evaluation of the scheme
 * function will actually end up calling "opencog-extension", which
 * passes this smob to do_call(). It will be do_call that then calls
 * the actual C++ function.
 *
 * Note that this method must be called in "guile mode".
 */
void
PrimitiveEnviron::really_do_register(const char * module_name,
                                     const char *name, int nargs,
                                     bool xport)
{
	init();

	// Scheme garbage collection will be managing the lifecycle
	scm_gc_register_allocation (get_size());

	// The (opencog extension) module
	std::string modn = "opencog ";
	modn += module_name;
	SCM module = scm_c_define_module(modn.c_str(), NULL, NULL);
	scm_c_use_module(modn.c_str());

	// The smob will hold a pointer to "this" -- the PrimitiveEnviron
	SCM smob;
	SCM_NEWSMOB (smob, SchemeSmob::cog_misc_tag, this);
	SCM_SET_SMOB_FLAGS(smob, SchemeSmob::COG_EXTEND);

	// We need to give the smob a unique name. Using addr of this is
	// sufficient for this purpose.
#define BUFLEN 40
	char buff[BUFLEN];
	snprintf(buff, BUFLEN, "cog-prim-%p", this);
	scm_c_module_define(module, buff, smob);

	std::string wrapper = "(use-modules (opencog extension))";
	if (xport)
		wrapper += "(define-public (";
	else
		wrapper += "(define (";
	wrapper += name;
	for (int i=0; i<nargs; i++)
	{
		wrapper += " ";
		char arg = 'a' + i;
		wrapper += arg;
	}
	wrapper += ") (opencog-extension ";
	wrapper += buff;
	wrapper += " (list";
	for (int i=0; i<nargs; i++)
	{
		wrapper += " ";
		char arg = 'a' + i;
		wrapper += arg;
	}
	wrapper += ")))";
	scm_c_eval_string_in_module(wrapper.c_str(), module);
	// printf("Debug: do_regsiter %s\n", wrapper.c_str());
}

SCM PrimitiveEnviron::do_call(SCM sfe, SCM arglist)
{
	// First, get the environ.
	PrimitiveEnviron *fe = verify_pe(sfe, "opencog-extension");

	SCM rc = SCM_EOL;

	// If the C++ code throws any exceptions, and no one else
	// has caught them, then we have to catch them, and print
	// an error message to the shell. Actually, we'll be nice
	// about this, and convert the C++ exception into a scheme
	// exception. If the exception is an OpenCog exception, then
	// we can have a stack trace. If its some other C++ exception,
	// then there is no stack trace, and we would need to overload
	// __cxa_throw() to get it to work. Yuck, so we don't do that.
	// Use gdb if you hit this situation.
	try
	{
		rc = fe->invoke(arglist);
	}
	catch (const std::exception& ex)
	{
		SchemeSmob::throw_exception(ex, fe->get_name(), arglist);
	}
	catch (...)
	{
		std::exception ex;
		SchemeSmob::throw_exception(ex, fe->get_name(), arglist);
	}
	scm_remember_upto_here_1(sfe);
	return rc;
}

PrimitiveEnviron * PrimitiveEnviron::verify_pe(SCM spe, const char *subrname)
{
	if (not SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, spe))
		scm_wrong_type_arg_msg(subrname, 1, spe, "opencog primitive function");

	scm_t_bits misctype = SCM_SMOB_FLAGS(spe);
	if (SchemeSmob::COG_EXTEND != misctype)
		scm_wrong_type_arg_msg(subrname, 1, spe, "opencog primitive function");

	PrimitiveEnviron * pe = (PrimitiveEnviron *) SCM_SMOB_DATA(spe);
	return pe;
}

