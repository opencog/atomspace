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
/// part must have N values.  Furthermore, any type restrictions on
/// the variables must be satisfied by the values.
///
/// The following formats are understood:
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
/// this trick cannot work for N=1 unless the variable is cosntrained
/// to not be a set.
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
	extract_variables();
	typecheck_values();
}

/// Extract the variables in the body.
/// The body must either be a lambda, or we assume all free variables get bound.
void PutLink::extract_variables(void)
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
}

/// Check that the values in the PutLink obey the type constraints.
void PutLink::typecheck_values(void)
{
	const Handle& vals = _outgoing[1];

	size_t sz = _varlist.varseq.size();
	Type vtype = vals->getType();

	if (1 == sz)
	{
		if (not _varlist.is_type(vals)
		    and SET_LINK != vtype)
		{
				throw InvalidParamException(TRACE_INFO,
					"PutLink mismatched type!");
		}
		return;
	}

	LinkPtr lval(LinkCast(vals));
	if (LIST_LINK == vtype)
	{
		if (not _varlist.is_type(lval->getOutgoingSet()))
			throw InvalidParamException(TRACE_INFO,
				"PutLink has mismatched value list!");
		return;
	}

	if (SET_LINK != vtype)
		throw InvalidParamException(TRACE_INFO,
			"PutLink was expecting a ListLink or SetLink!");

	if (1 < sz)
	{
		for (const Handle& h : lval->getOutgoingSet())
		{
			LinkPtr lse(LinkCast(h));
			// If the arity is greater than one, then the values must be in a list.
		   if (lse->getType() != LIST_LINK)
				throw InvalidParamException(TRACE_INFO,
					"PutLink expected value list!");

			if (not _varlist.is_type(lval->getOutgoingSet()))
				throw InvalidParamException(TRACE_INFO,
					"PutLink bad value list!");
		}
		return;
	}

	// If the arity is one, the values must obey type constraint.
	for (const Handle& h : lval->getOutgoingSet())
	{
		if (not _varlist.is_type(h))
			throw InvalidParamException(TRACE_INFO,
					"PutLink bad type!");
	}
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
Handle PutLink::do_reduce(void) const
{
	const Handle& body = _outgoing[0];
	const Handle& vals = _outgoing[1];

	if (1 == _varlist.varseq.size())
	{
		HandleSeq oset;
		oset.push_back(vals);
		return _varlist.substitute_nocheck(body, oset);
	}
	if (vals->getType() == LIST_LINK)
	{
		const HandleSeq& oset = LinkCast(vals)->getOutgoingSet();
		return _varlist.substitute_nocheck(body, oset);
	}

	OC_ASSERT(vals->getType() == SET_LINK,
		"Should have caught this earlier, in the ctor");

	HandleSeq bset;
	for (Handle h : LinkCast(vals)->getOutgoingSet())
	{
		const HandleSeq& oset = LinkCast(h)->getOutgoingSet();
		bset.push_back(_varlist.substitute_nocheck(body, oset));
	}
	return Handle(createLink(SET_LINK, bset));
}

Handle PutLink::reduce(void)
{
	return do_reduce();
}

/* ===================== END OF FILE ===================== */
