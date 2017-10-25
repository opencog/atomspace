/*
 * opencog/atoms/core/PutLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
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

#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include "DefineLink.h"
#include "FreeLink.h"
#include "LambdaLink.h"
#include "PutLink.h"

using namespace opencog;

PutLink::PutLink(const HandleSeq& oset, Type t)
    : ScopeLink(oset, t)
{
	init();
}

PutLink::PutLink(const Handle& a)
    : ScopeLink(PUT_LINK, a)
{
	init();
}

PutLink::PutLink(const Link& l)
    : ScopeLink(l)
{
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
	if (not classserver().isA(get_type(), PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");

	size_t sz = _outgoing.size();
	if (2 != sz and 3 != sz)
		throw InvalidParamException(TRACE_INFO,
			"Expecting an outgoing set size of two or three, got %d; %s",
			sz, to_string().c_str());

	ScopeLink::extract_variables(_outgoing);

	if (2 == sz)
	{
		// If the body is just a single variable, and there are no
		// type declarations for it, then ScopeLink gets confused.
		_vardecl = Handle::UNDEFINED;
		_body = _outgoing[0];
		_values = _outgoing[1];
	}
	else
		_values = _outgoing[2];

	static_typecheck_values();
}


/// Check that the values in the PutLink obey the type constraints.
/// This only performs "static" typechecking, at construction-time;
/// since the values may be dynamically obtained at run-time, we cannot
/// check these here.
void PutLink::static_typecheck_values(void)
{
	// Cannot typecheck at this pont in time, because the schema
	// might not be defined yet...
	Type btype = _body->get_type();
	if (DEFINED_SCHEMA_NODE == btype)
		return;
	if (DEFINED_PREDICATE_NODE == btype)
		return;

	// If its part of a signature, there is nothing to do.
	if (classserver().isA(btype, TYPE_NODE) or TYPE_CHOICE == btype)
		return;

	size_t sz = _varlist.varseq.size();
	Type vtype = _values->get_type();

	if (1 == sz)
	{
		if (not _varlist.is_type(_values)
		    and SET_LINK != vtype
		    and PUT_LINK != vtype
		    and not (classserver().isA(vtype, SATISFYING_LINK)))
		{
				throw InvalidParamException(TRACE_INFO,
					"PutLink mismatched type!");
		}
		return;
	}

	// The standard, default case is to get a ListLink as an argument.
	if (LIST_LINK == vtype)
	{
		if (not _varlist.is_type(_values->getOutgoingSet()))
		{
			if (_vardecl)
				throw SyntaxException(TRACE_INFO,
					"PutLink has mismatched value list! vardecl=%s\nvals=%s",
					_vardecl->to_string().c_str(),
					_values->to_string().c_str());
			else
				throw SyntaxException(TRACE_INFO,
					"PutLink has mismatched value list! body=%s\nvals=%s",
					_body->to_string().c_str(),
					_values->to_string().c_str());
		}
		return;
	}

	// GetLinks (and the like) are evaluated dynamically, later.
	if (classserver().isA(vtype, SATISFYING_LINK))
		return;

	// If its part of a signature, there is nothing to do.
	if (TYPE_NODE == vtype or TYPE_CHOICE == vtype)
		return;

	// The only remaining possibility is that there is set of ListLinks.
	if (SET_LINK != vtype)
		throw InvalidParamException(TRACE_INFO,
			"PutLink was expecting a ListLink, SetLink or GetLink!");

	if (1 < sz)
	{
		for (const Handle& h : _values->getOutgoingSet())
		{
			// If the arity is greater than one, then the values must be in a list.
		   if (h->get_type() != LIST_LINK)
				throw InvalidParamException(TRACE_INFO,
					"PutLink expected value list!");

			if (not _varlist.is_type(h->getOutgoingSet()))
				throw InvalidParamException(TRACE_INFO,
					"PutLink bad value list!");
		}
		return;
	}

	// If the arity is one, the values must obey type constraint.
	for (const Handle& h : _values->getOutgoingSet())
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
 * have the desired types, no substitution is performed.  In this case,
 * an undefined handle is returned. For set substitutions, this acts as
 * a filter, removing (filtering out) the mismatched types.
 *
 * Again, only a substitution is performed, there is no execution or
 * evaluation.  Note also that the resulting tree is NOT placed into
 * any atomspace!
 */
Handle PutLink::do_reduce(void) const
{
	Handle bods(_body);
	Variables vars(_varlist);
	// Resolve the body, if needed. That is, if the body is
	// given in a defintion, get that defintion.
	Type btype = _body->get_type();
	if (DEFINED_SCHEMA_NODE == btype or
	    DEFINED_PREDICATE_NODE == btype)
	{
		bods = DefineLink::get_definition(bods);
		btype = bods->get_type();
		// XXX TODO we should perform a type-check on the function.
		if (not classserver().isA(btype, LAMBDA_LINK))
			throw InvalidParamException(TRACE_INFO,
					"Expecting a LambdaLink, got %s",
			      bods->to_string().c_str());
	}

	// If the body is a lambda, work with that.
	if (classserver().isA(btype, LAMBDA_LINK))
	{
		LambdaLinkPtr lam(LambdaLinkCast(bods));
		bods = lam->get_body();
		vars = lam->get_variables();
	}

	// Now get the values that we will plug into the body.
	Type vtype = _values->get_type();

	// If there is only one variable in the PutLink body...
	if (1 == vars.varseq.size())
	{
		if (SET_LINK != vtype)
		{
			HandleSeq oset;
			oset.emplace_back(_values);
			try
			{
				return vars.substitute(bods, oset, /* silent */ true);
			}
			catch (const TypeCheckException& ex)
			{
				return Handle::UNDEFINED;
			}
		}

		// If the values are given in a set, then iterate over the set...
		HandleSeq bset;
		for (const Handle& h : _values->getOutgoingSet())
		{
			HandleSeq oset;
			oset.emplace_back(h);
			try
			{
				bset.emplace_back(vars.substitute(bods, oset, /* silent */ true));
			}
			catch (const TypeCheckException& ex) {}
		}
		return createLink(bset, SET_LINK);
	}

	// If we are here, then there are multiple variables in the body.
	// See how many values there are.  If the values are a ListLink,
	// then assume that there is only a single set of values to plug in.
	if (LIST_LINK == vtype)
	{
		const HandleSeq& oset = _values->getOutgoingSet();
		try
		{
			return vars.substitute(bods, oset, /* silent */ true);
		}
		catch (const TypeCheckException& ex)
		{
			return Handle::UNDEFINED;
		}
	}

	// If we are here, then there are multiple values.
	// These  MUST be given to us as a SetLink.
	OC_ASSERT(SET_LINK == vtype,
		"Should have caught this earlier, in the ctor");

	HandleSeq bset;
	for (const Handle& h : _values->getOutgoingSet())
	{
		const HandleSeq& oset = h->getOutgoingSet();
		try
		{
			bset.emplace_back(vars.substitute(bods, oset, /* silent */ true));
		}
		catch (const TypeCheckException& ex) {}
	}
	return createLink(bset, SET_LINK);
}

Handle PutLink::reduce(void)
{
	return do_reduce();
}

DEFINE_LINK_FACTORY(PutLink, PUT_LINK)

/* ===================== END OF FILE ===================== */
