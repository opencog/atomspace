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
#include "DefineLink.h"
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
	size_t sz = _outgoing.size();
	if (2 != sz)
		throw InvalidParamException(TRACE_INFO,
			"Expecting an outgoing set size of two, got %d", sz);

	// There are two situations that can occur here:
	// The first atom is LambdaLink. In this case, just use it.
	// The first atom is just a some free-form expression. In that
	// grab all the free variables in that, and just use them.
	Handle putty(_outgoing[0]);
	if (classserver().isA(putty->getType(), LAMBDA_LINK))
	{
		LambdaLinkPtr lam(LambdaLinkCast(putty));
		if (nullptr == lam)
			lam = createLambdaLink(*LinkCast(putty));

		_varlist = lam->get_variables();
		_body = lam->get_body();
	}
	else
	{
		FreeLink fl(putty);
		VariableList vl(fl.get_vars());
		_varlist = vl.get_variables();
		_body = putty;
	}

	_values = _outgoing[1];
	static_typecheck_values();
}


/// Check that the values in the PutLink obey the type constraints.
/// This only performs "static" typechecking, at construction-time;
/// the values may be dynamically obtained at run-time, we cannot check
/// these here.
void PutLink::static_typecheck_values(void)
{
	// Cannot typecheck at this pont in time, because the schema
	// might not be defined yet...
	Type btype = _body->getType();
	if (DEFINED_SCHEMA_NODE == btype)
		return;

	size_t sz = _varlist.varseq.size();
	Type vtype = _values->getType();

	if (1 == sz)
	{
		if (not _varlist.is_type(_values)
		    and SET_LINK != vtype)
		{
				throw InvalidParamException(TRACE_INFO,
					"PutLink mismatched type!");
		}
		return;
	}

	LinkPtr lval(LinkCast(_values));
	if (LIST_LINK == vtype)
	{
		if (not _varlist.is_type(lval->getOutgoingSet()))
			throw InvalidParamException(TRACE_INFO,
				"PutLink has mismatched value list!");
		return;
	}

	// GetLinks are evaluated dynamically, later.
	if (GET_LINK == vtype)
		return;

	if (SET_LINK != vtype)
		throw InvalidParamException(TRACE_INFO,
			"PutLink was expecting a ListLink, SetLink or GetLink!");

	if (1 < sz)
	{
		for (const Handle& h : lval->getOutgoingSet())
		{
			LinkPtr lse(LinkCast(h));
			// If the arity is greater than one, then the values must be in a list.
		   if (lse->getType() != LIST_LINK)
				throw InvalidParamException(TRACE_INFO,
					"PutLink expected value list!");

			if (not _varlist.is_type(lse->getOutgoingSet()))
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
 * Type checking is performed during substitution; if the values fail to
 * have the desired types, no substituion is performed.  In this case,
 * an undefined handle is returned. For set substitutions, this acts as
 * a filter, removeing (filtering out) the mismatched types.
 *
 * Again, only a substitution is performed, there is no evaluation.
 * Note also that the resulting tree is NOT placed into any atomspace!
 */
Handle PutLink::do_reduce(void) const
{
	Handle bods(_body);
	Variables vars(_varlist);
	// Resolve the body, if needed:
	if (DEFINED_SCHEMA_NODE == _body->getType())
	{
		Handle dfn(DefineLink::get_definition(_body));
		// XXX TODO we should perform a type-check on the function.
		if (not classserver().isA(dfn->getType(), LAMBDA_LINK))
			throw InvalidParamException(TRACE_INFO,
					"Expecting a LambdaLink, got %s",
			      dfn->toString().c_str());

		LambdaLinkPtr lam(LambdaLinkCast(dfn));
		if (NULL == lam)
			lam = createLambdaLink(*LinkCast(dfn));
		bods = lam->get_body();
		vars = lam->get_variables();
	}

	Type vtype = _values->getType();

	if (1 == vars.varseq.size())
	{
		if (SET_LINK != vtype)
		{
			HandleSeq oset;
			oset.emplace_back(_values);
			try
			{
				return vars.substitute(bods, oset);
			}
			catch (...)
			{
				return Handle::UNDEFINED;
			}
		}

		// Iterate over the set...
		HandleSeq bset;
		for (Handle h : LinkCast(_values)->getOutgoingSet())
		{
			HandleSeq oset;
			oset.emplace_back(h);
			try
			{
				bset.emplace_back(vars.substitute(bods, oset));
			}
			catch (...) {}
		}
		return Handle(createLink(SET_LINK, bset));
	}
	if (LIST_LINK == vtype)
	{
		const HandleSeq& oset = LinkCast(_values)->getOutgoingSet();
		try
		{
			return vars.substitute(bods, oset);
		}
		catch (...)
		{
			return Handle::UNDEFINED;
		}
	}

	OC_ASSERT(SET_LINK == vtype,
		"Should have caught this earlier, in the ctor");

	HandleSeq bset;
	for (Handle h : LinkCast(_values)->getOutgoingSet())
	{
		const HandleSeq& oset = LinkCast(h)->getOutgoingSet();
		try
		{
			bset.emplace_back(vars.substitute(bods, oset));
		}
		catch (...) {}
	}
	return Handle(createLink(SET_LINK, bset));
}

Handle PutLink::reduce(void)
{
	return do_reduce();
}

/* ===================== END OF FILE ===================== */
