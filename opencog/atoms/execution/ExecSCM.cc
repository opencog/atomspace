/*
 * ExecSCM.cc
 *
 * Guile Scheme bindings for the execution links
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */


#include <cstddef>
#include <opencog/atomspace/Link.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/reduct/FunctionLink.h>
#include <opencog/guile/SchemeModule.h>

#include "ExecSCM.h"


// ========================================================

using namespace opencog;

/**
 * cog-execute! executes an ExecutionOutputLink
 */
static Handle ss_execute(AtomSpace* atomspace, const Handle& h)
{
	Instantiator inst(atomspace);
	Handle rh(inst.execute(h));
	if (NULL != rh)
		rh = atomspace->add_atom(rh);
	return rh;
}

/**
 * cog-evaluate! evaluates an EvaluationLink with a GPN in it.
 */
static TruthValuePtr ss_evaluate(AtomSpace* atomspace, const Handle& h)
{
	return EvaluationLink::do_evaluate(atomspace, h);
}

/**
 * cog-reduce! reduces a FreeLink with free variables in it.
 */
static Handle ss_reduce(AtomSpace* atomspace, const Handle& h)
{
	Type t = h->getType();
	if (NUMBER_NODE == t) return Handle(h);

	if (not classserver().isA(t, FREE_LINK))
	{
		throw InvalidParamException(TRACE_INFO,
			"Expecting a FreeLink (PlusLink, TimesLink, etc");
	}

	// Arghh.  The cast should have been enough, but we currently
	// can't store these in the atomsapce, due to circular shared
	// lib dependencies.
	FunctionLinkPtr fff(FunctionLinkCast(h));
	if (NULL == fff)
		fff = FunctionLinkCast(FunctionLink::factory(LinkCast(h)));
	Handle hr(fff->reduce());

	if (DELETE_LINK == hr->getType())
	{
		for (const Handle& ho : LinkCast(hr)->getOutgoingSet())
			atomspace->remove_atom(ho, true);
		return Handle::UNDEFINED;
	}

	return atomspace->add_atom(hr);
}

// ========================================================

// XXX HACK ALERT This needs to be static, in order for python to
// work correctly.  The problem is that python keeps creating and
// destroying this class, but it expects things to stick around.
// Oh well. I guess that's OK, since the definition is meant to be
// for the lifetime of the server, anyway.
std::vector<FunctionWrap*> ExecSCM::_binders;

ExecSCM::ExecSCM(void) :
	ModuleWrap("opencog exec")
{}

/// This is called while (opencog exec) is the current module.
/// Thus, all the definitions below happen in that module.
void ExecSCM::init(void)
{
	_binders.push_back(new FunctionWrap(ss_execute,
	                    "cog-execute!", "exec"));

	_binders.push_back(new FunctionWrap(ss_evaluate,
	                   "cog-evaluate!", "exec"));

	_binders.push_back(new FunctionWrap(ss_reduce,
	                   "cog-reduce!", "exec"));
}

ExecSCM::~ExecSCM()
{
#if PYTHON_BUG_IS_FIXED
	for (FunctionWrap* pw : _binders)
		delete pw;
#endif
}


void opencog_exec_init(void)
{
	static ExecSCM exy;
	exy.module_init();
}
