/*
 * opencog/atoms/core/PutLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Put Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Put Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/ClassServer.h>
#include "FreeLink.h"
#include "LambdaLink.h"
#include "PutLink.h"

using namespace opencog;

PutLink::PutLink(const HandleSeq& oset,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : Link(PUT_LINK, oset, tv, av)
{
	init();
}

PutLink::PutLink(const Handle& a,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : Link(PUT_LINK, a, tv, av)
{
	init();
}

PutLink::PutLink(Type t, const HandleSeq& oset,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : Link(t, oset, tv, av)
{
	if (not classserver().isA(t, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

PutLink::PutLink(Type t, const Handle& a,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : Link(t, a, tv, av)
{
	if (not classserver().isA(t, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

PutLink::PutLink(Type t, const Handle& a, const Handle& b,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : Link(t, a, b, tv, av)
{
	if (not classserver().isA(t, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

PutLink::PutLink(Link& l)
    : Link(l)
{
	Type tscope = l.getType();
	if (not classserver().isA(tscope, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

/* ================================================================= */

/// PutLink expects a very strict format: an arity-2 link, with
/// the first part being a pattern, and the second a list or set
/// of values. If the pattern has N variables, then the seccond
/// part must have N values.  The following formats are understood:
///
///    PutLink
///       <pattern with 1 variable>
///       <any single atom>
///
///    PutLink
///       <pattern with N variables>
///       ListLink     ;; must have arity N
///          <atom 1>
///          ...
///          <atom N>
///
/// The below is a handy-dandy easy-to-use form. When it is reduced,
/// it will result in the creation of a set of reduced forms, not
/// just one (the two sets haveing the same arity). Unfortunately,
/// this trick cannot work for N=1.
///
///    PutLink
///       <pattern with N variables>
///       SetLink        ;; Must hold a set of ListLinks
///          ListLink    ;; must have arity N
///             <atom 1>
///             ...
///             <atom N>
///
void PutLink::init(void)
{
	size_t sz = _outgoing.size();
	if (2 != sz)
		throw InvalidParamException(TRACE_INFO, "Unexprected PutLink arity! Got %lu", sz);

	const Handle& body = _outgoing[0];
	Type btype = body->getType();

	// If the body is a LambdaLink, then use it's variable declarations;
	// else use the FreeLink to find all the variables.
	if (classserver().isA(btype, LAMBDA_LINK))
	{
		LambdaLinkPtr lam(LambdaLinkCast(body));
		if (NULL == lam)
			lam = createLambdaLink(*LinkCast(body));
		_varlist = lam->get_variables();
	}
	else
	{
		FreeLink fl(body);
		VariableList vl(fl.get_vars());
		_varlist = vl.get_variables();
	}

#if LATER
	// OK, now for the values.
	if (_varseq.size() == 1) return;

	LinkPtr lval(LinkCast(_outgoing[1]));
	if (lval->getType() == LIST_LINK)
	{
		if (lval->getArity() != _varseq.size())
			throw InvalidParamException(TRACE_INFO,
				"PutLink has mismatched size! Expected %zu, got %zu\n",
				_varseq.size(), lval->getArity());
		return;
	}
	if (lval->getType() != SET_LINK)
		throw InvalidParamException(TRACE_INFO,
			"PutLink was expecting a ListLink or SetLink!");

	for (const Handle& h : lval->getOutgoingSet())
	{
		LinkPtr lse(LinkCast(h));
		if (lse->getType() != LIST_LINK)
			throw InvalidParamException(TRACE_INFO,
				"PutLink was expecting a ListLink here");
		if (lse->getArity() != _varseq.size())
			throw InvalidParamException(TRACE_INFO,
				"PutLink set element has mismatched size! Expected %zu, got %zu\n",
				_varseq.size(), lse->getArity());
	}
#endif
}
/* ================================================================= */

/**
 * Perform the actual beta reduction --
 *
 * Substitute values for the variables in the pattern tree.
 * This is a lot like applying the function fun to the argument list
 * args, except that no actual evaluation is performed; only
 * substitution.  The resulting tree is NOT placed into any atomspace,
 * either. If you want that, you must do it youself.  If you want
 * evaluation or execution to happen during or after sustitution, use
 * either the EvaluationLink, the ExecutionOutputLink, or the Instantiator.
 *
 * So, for example, if this PutLink looks like this:
 *
 *   PutLink
 *      EvaluationLink
 *         PredicateNode "is a kind of"
 *         ListLink
 *            VariableNode $a
 *            ConceptNode "hot patootie"
 *      ConceptNode "cowpie"
 *
 * then the reduced value will be
 *
 *   EvaluationLink
 *      PredicateNode "is a kind of"
 *      ListLink
 *         ConceptNode "cowpie"
 *         ConceptNode "hot patootie"
 *
 * Again, only a substitution is performed, there is no evaluation.
 * Note also that the resulting tree is NOT placed into any atomspace!
 */
Handle PutLink::substitute_nocheck(const Handle& term,
                                   const HandleSeq& args) const
{
#if 0
	// If it is a singleton, just return that singleton.
	std::map<Handle, unsigned int>::const_iterator idx;
	idx = _index.find(term);
	if (idx != _index.end())
		return args.at(idx->second);

	// If its a node, and its not a variable, then it is a constant,
	// and just return that.
	LinkPtr lterm(LinkCast(term));
	if (NULL == lterm) return term;

	// QuoteLinks halt the reursion
	if (QUOTE_LINK == term->getType()) return term;

	// Recursively fill out the subtrees.
	HandleSeq oset;
	for (const Handle& h : lterm->getOutgoingSet())
	{
		oset.push_back(substitute_nocheck(h, args));
	}
	return Handle(createLink(term->getType(), oset));
#endif
return Handle::UNDEFINED; // XXX tmp hack
}

Handle PutLink::do_reduce(void) const
{
	const Handle& body = _outgoing[0];
	const Handle& vals = _outgoing[1];

#if 0
	if (1 == _varseq.size())
	{
		HandleSeq oset;
		oset.push_back(vals);
		return substitute_nocheck(body, oset);
	}
	if (vals->getType() == LIST_LINK)
	{
		const HandleSeq& oset = LinkCast(vals)->getOutgoingSet();
		return substitute_nocheck(body, oset);
	}

	OC_ASSERT(vals->getType() == SET_LINK,
		"Should have checked for this earlier, tin the ctor");
#endif

	HandleSeq bset;
	for (Handle h : LinkCast(vals)->getOutgoingSet())
	{
		const HandleSeq& oset = LinkCast(h)->getOutgoingSet();
		bset.push_back(substitute_nocheck(body, oset));
	}
	return Handle(createLink(SET_LINK, bset));
}

Handle PutLink::reduce(void)
{
	return do_reduce();
}

/* ===================== END OF FILE ===================== */
