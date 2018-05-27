/*
 * ExecSCM.cc
 *
 * Guile Scheme bindings for the execution links
 * Copyright (c) 2008, 2014, 2015 Linas Vepstas <linas@linas.org>
 */

#ifdef HAVE_GUILE

#include <cstddef>
#include <opencog/atoms/base/Link.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/reduct/FoldLink.h>
#include <opencog/guile/SchemeModule.h>

#include "ExecSCM.h"

// ========================================================

using namespace opencog;

/**
 * cog-execute! executes any/all FunctionLinks
 */
static ProtoAtomPtr ss_execute(AtomSpace* atomspace, const Handle& h)
{
	Instantiator inst(atomspace);
	ProtoAtomPtr pap(inst.execute(h));
	Handle rh(HandleCast(pap));
	if (NULL != rh)
		pap = atomspace->add_atom(rh);
	return pap;
}

/**
 * cog-evaluate! evaluates an EvaluationLink with a GPN in it.
 */
static TruthValuePtr ss_evaluate(AtomSpace* atomspace, const Handle& h)
{
	return EvaluationLink::do_evaluate(atomspace, h);
}

// ========================================================

// XXX HACK ALERT This needs to be static, in order for python to
// work correctly.  The problem is that python keeps creating and
// destroying this class, but it expects things to stick around.
// Oh well. I guess that's OK, since the definition is meant to be
// for the lifetime of the server, anyway.
std::vector<FunctionWrap*>* ExecSCM::_binders = nullptr;

ExecSCM::ExecSCM(void) :
	ModuleWrap("opencog exec")
{}

/// This is called while (opencog exec) is the current module.
/// Thus, all the definitions below happen in that module.
void ExecSCM::init(void)
{
	_binders = new std::vector<FunctionWrap*>();
	_binders->push_back(new FunctionWrap(ss_execute,
	                   "cog-execute!", "exec"));

	_binders->push_back(new FunctionWrap(ss_evaluate,
	                   "cog-evaluate!", "exec"));
}

ExecSCM::~ExecSCM()
{
#if PYTHON_BUG_IS_FIXED
	for (FunctionWrap* pw : *_binders)
		delete pw;
	delete _binders;
#endif
}


void opencog_exec_init(void)
{
	static ExecSCM exy;
	exy.module_init();
}
#endif // HAVE_GUILE
