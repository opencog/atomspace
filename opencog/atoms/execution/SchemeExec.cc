/*
 * SchemeExec.cc
 *
 * Execute ExecutionOutputLinks, EvaluationLinks with GPN's.
 * Copyright (c) 2009,2015 Linas Vepstas <linasvepstas@gmail.com>
 */

#ifdef HAVE_GUILE

#include <cstddef>
#include <libguile.h>
#include <opencog/atomspace/Link.h>
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/execution/Instantiator.h>

#include "SchemeSmob.h"

using namespace opencog;


/**
 * Executes an ExecutionOutputLink
 */
SCM SchemeSmob::ss_execute (SCM satom)
{
	Handle h = verify_handle(satom, "cog-execute!");
	AtomSpace* atomspace = ss_get_env_as("cog-execute!");
	Instantiator inst(atomspace);

	// execute() may throw a C++ exception in various cases:
	// e.g. if it names a non-existant function, or a function
	// with syntax errors.
	try
	{
		h = inst.execute(h);
		if (NULL != h)
			h = atomspace->addAtom(h);
		return handle_to_scm(h);
	}
	catch (const std::exception& ex)
	{
		SchemeSmob::throw_exception(ex.what(), "cog-execute!");
	}
	catch (...)
	{
		SchemeSmob::throw_exception(NULL, "cog-execute!");
	}
	scm_remember_upto_here_1(satom);
	return SCM_EOL;
}

/**
 * Executes an EvaluationLink with a GPN in it.
 */
SCM SchemeSmob::ss_evaluate (SCM satom)
{
	Handle h = verify_handle(satom, "cog-evaluate!");

	Type t = h->getType();
	if ((EVALUATION_LINK != t)
	    and (NOT_LINK != t)
	    and (not classserver().isA(t, VIRTUAL_LINK)))
	{
		scm_wrong_type_arg_msg("cog-evaluate!", 1, satom,
			"EvaluationLink or NotLink or VirtualLink");
	}

	AtomSpace* atomspace = ss_get_env_as("cog-evaluate!");
	// do_evaluate() may throw a C++ exception in various cases:
	// e.g. if it names a non-existant function, or a function
	// with syntax errors, or if the scheme code intentionally
	// threw an error.
	try
	{
		TruthValuePtr tvp = EvaluationLink::do_evaluate(atomspace, h);
		return take_tv(tvp->rawclone());
	}
	catch (const std::exception& ex)
	{
		SchemeSmob::throw_exception(ex.what(), "cog-evaluate!");
	}
	catch (...)
	{
		SchemeSmob::throw_exception(NULL, "cog-evaluate!");
	}
	scm_remember_upto_here_1(satom);
	return SCM_EOL;
}

/**
 * Reduces a FreeLink with free variables in it.
 */
SCM SchemeSmob::ss_reduce (SCM satom)
{
	Handle h = verify_handle(satom, "cog-reduce!");

	Type t = h->getType();
	if (NUMBER_NODE == t) return satom;

	if (not classserver().isA(t, FREE_LINK))
	{
		scm_wrong_type_arg_msg("cog-reduce!", 1, satom,
			"FreeLink (PlusLink, TimesLink, etc");
	}

	// do_reduce() may throw a C++ exception, usually because the
	// expression contains non-reducible atoms in it.
	try
	{
		AtomSpace* atomspace = ss_get_env_as("reduce!");
		FreeLinkPtr fff(FreeLinkCast(h));
		Handle hr(fff->reduce());

		if (DELETE_LINK == hr->getType())
		{
			for (const Handle& ho : LinkCast(hr)->getOutgoingSet())
				atomspace->removeAtom(ho, true);
			return handle_to_scm(Handle::UNDEFINED);
		}
		hr = atomspace->addAtom(hr);
		return handle_to_scm(hr);
	}
	catch (const std::exception& ex)
	{
		SchemeSmob::throw_exception(ex.what(), "cog-reduce!");
	}
	catch (...)
	{
		SchemeSmob::throw_exception(NULL, "cog-reduce!");
	}
	scm_remember_upto_here_1(satom);
	return SCM_EOL;
}

#endif
/* ===================== END OF FILE ============================ */
