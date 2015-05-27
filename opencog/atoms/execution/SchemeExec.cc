/*
 * SchemeExec.cc
 *
 * Execute ExecutionOutputLinks, EvaluationLinks with GPN's.
 * Copyright (c) 2009,2015 Linas Vepstas <linasvepstas@gmail.com>
 */


#include <cstddef>
#include <opencog/atomspace/Link.h>
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/execution/Instantiator.h>

using namespace opencog;

/**
 * cog-execute! executes an ExecutionOutputLink
 */
Handle ss_execute(AtomSpace* atomspace, const Handle& h)
{
	Instantiator inst(atomspace);
	Handle rh(inst.execute(h));
	if (NULL != rh)
		rh = atomspace->addAtom(rh);
	return rh;
}

/**
 * cog-evaluate! evaluates an EvaluationLink with a GPN in it.
 */
TruthValuePtr ss_evaluate(AtomSpace* atomspace, const Handle& h)
{
	return EvaluationLink::do_evaluate(atomspace, h);
}

/**
 * cog-reduce! reduces a FreeLink with free variables in it.
 */
Handle ss_reduce(AtomSpace* atomspace, const Handle& h)
{
	Type t = h->getType();
	if (NUMBER_NODE == t) return Handle(h);

	if (not classserver().isA(t, FREE_LINK))
	{
		throw InvalidParamException(TRACE_INFO,
			"Expecteing a FreeLink (PlusLink, TimesLink, etc");
	}

	FreeLinkPtr fff(FreeLinkCast(h));
	Handle hr(fff->reduce());

	if (DELETE_LINK == hr->getType())
	{
		for (const Handle& ho : LinkCast(hr)->getOutgoingSet())
			atomspace->removeAtom(ho, true);
		return Handle::UNDEFINED;
	}

	return atomspace->addAtom(hr);
}

/* ===================== END OF FILE ============================ */
