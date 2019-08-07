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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include "DefineLink.h"
#include "LambdaLink.h"
#include "PutLink.h"
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

PutLink::PutLink(const HandleSeq& oset, Type t)
    : PrenexLink(oset, t)
{
	init();
}

PutLink::PutLink(const Link& l)
    : PrenexLink(l)
{
	init();
}

/* ================================================================= */

/// PutLink expects a very strict format: an arity-2 link, with
/// the first part being a pattern, and the second a list or set
/// of arguments. If the pattern has N variables, then the seccond
/// part must have N arguments.  Furthermore, any type restrictions
/// on the variables must be satisfied by the arguments.
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
	if (not nameserver().isA(get_type(), PUT_LINK))
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
		_arguments = _outgoing[1];
	}
	else
		_arguments = _outgoing[2];

	static_typecheck_arguments();
}


/// Check that the arguments in the PutLink obey the type constraints.
/// This only performs "static" typechecking, at construction-time;
/// since the arguments may be dynamically obtained at run-time, we cannot
/// check these here.
void PutLink::static_typecheck_arguments(void)
{
	// Cannot typecheck at this pont in time, because the schema
	// might not be defined yet...
	Type btype = _body->get_type();
	if (DEFINED_SCHEMA_NODE == btype)
		return;
	if (DEFINED_PREDICATE_NODE == btype)
		return;
	if (PUT_LINK == btype)
		return;

	// If its part of a signature, there is nothing to do.
	if (nameserver().isA(btype, TYPE_NODE) or TYPE_CHOICE == btype)
		return;

	Handle valley = _arguments;
	Type vtype = valley->get_type();

	// If it's body or argument is an UnquoteLink then the PutLink is
	// likely quoted and thus there is nothing to do
	if (btype == UNQUOTE_LINK or vtype == UNQUOTE_LINK)
		return;

	// If its a LambdaLink, try to see if its eta-reducible. If
	// so, then eta-reduce it immediately. A LambdaLink is
	// eta-reducible when it's body is a ListLink.
	if (LAMBDA_LINK == vtype)
	{
		LambdaLinkPtr lam(LambdaLinkCast(_arguments));
		Handle body = lam->get_body();

		// The body might not exist, if there's an unmantched
		// UnquoteLink in it.  I really dislike Quote/Unquote.
		// There's something deeply evil about them.
		if (nullptr == body)
			return;

		Type bt = body->get_type();
		if (LIST_LINK == bt)
		{
			valley = body;
			vtype = bt;
		}
	}

	size_t sz = _varlist.varseq.size();
	if (1 == sz)
	{
		if (not _varlist.is_type(valley)
		    and SET_LINK != vtype
		    and PUT_LINK != vtype
		    and not (nameserver().isA(vtype, SATISFYING_LINK)))
		{
			// Well, one more possible case ...
			// Function composition with lambda means that
			// the body of the lambda must be the right type.
			if (LAMBDA_LINK == vtype)
			{
				LambdaLinkPtr lam(LambdaLinkCast(valley));
				const Handle& body = lam->get_body();
				if (_varlist.is_type(body))
					return; // everything is OK.
			}

			throw InvalidParamException(TRACE_INFO,
				"PutLink mismatched type!");
		}
		return;
	}

	// Cannot typecheck naked FunctionLinks.  For example:
	// (cog-execute! (Put (Plus) (List (Number 2) (Number 2))))
	if (0 == sz and nameserver().isA(btype, FUNCTION_LINK))
		return;

	// The standard, default case is to get a ListLink as an argument.
	if (LIST_LINK == vtype)
	{
		// is_type() verifies that the arity of the vars
		// and the arguments matches up.
		if (not _varlist.is_type(valley->getOutgoingSet()))
		{
			if (_vardecl)
				throw SyntaxException(TRACE_INFO,
					"PutLink has mismatched argument list! vardecl=%s\nvals=%s",
					_vardecl->to_string().c_str(),
					_arguments->to_string().c_str());
			else
				throw SyntaxException(TRACE_INFO,
					"PutLink has mismatched argument list! body=%s\nvals=%s",
					_body->to_string().c_str(),
					_arguments->to_string().c_str());
		}
		return;
	}

	// GetLinks (and the like) are evaluated dynamically, later.
	if (nameserver().isA(vtype, SATISFYING_LINK))
		return;

	// If its part of a signature, there is nothing to do.
	if (TYPE_NODE == vtype or TYPE_CHOICE == vtype)
		return;

	// The only remaining possibility is that there is set of ListLinks.
	if (SET_LINK != vtype)
		throw InvalidParamException(TRACE_INFO,
			"PutLink was expecting a ListLink, SetLink or GetLink!");

	if (0 == sz)
		throw InvalidParamException(TRACE_INFO,
			"PutLink: cannot put the empty set!");

	if (1 < sz)
	{
		for (const Handle& h : valley->getOutgoingSet())
		{
			// If the arity is greater than one,
			// then the arguments must be in a list.
			if (h->get_type() != LIST_LINK)
				throw InvalidParamException(TRACE_INFO,
					"PutLink expected argument list!");

			if (not _varlist.is_type(h->getOutgoingSet()))
				throw InvalidParamException(TRACE_INFO,
					"PutLink bad argument list!");
		}
		return;
	}

	// If the arity is one, the arguments must obey type constraint.
	for (const Handle& h : valley->getOutgoingSet())
	{
		if (not _varlist.is_type(h))
			throw InvalidParamException(TRACE_INFO,
					"PutLink bad type!");
	}
}

/* ================================================================= */

static inline Handle reddy(PrenexLinkPtr& subs, const HandleSeq& oset)
{
	subs->make_silent(true);
	return subs->beta_reduce(oset);
}

// If arg is executable, then run it, and unwrap the set link, too.
// We unwrap the SetLinks cause that is what GetLinks return.
static inline Handle expand(const Handle& arg, bool silent)
{
	Handle result(arg);
	if (arg->is_executable())
		result = HandleCast(arg->execute());

	if (SET_LINK == result->get_type())
	{
		Arity n = result->get_arity();
		if (1 == n)
			result = result->getOutgoingAtom(0);

		if (0 == n)
		{
			if (silent) throw SilentException();
			throw RuntimeException(TRACE_INFO, "Cannot put the empty set!");
		}
	}
	return result;
}

/**
 * Perform the actual beta reduction --
 *
 * Substitute arguments for the variables in the pattern tree.
 * This is not a pure beta-reduction, because it does execute
 * any executable arguments, so as to obtain the correct forms
 * to place into the reduction.  This does make the PutLink
 * resemble function application; however, here, the application
 * is not infinite-recursive; it is only one level deep.  There
 * is just enough execution performed to get the neeed arguments,
 * and no more.
 *
 * What this actually does is fairly complex and sophisticated.
 * First, when LambdaLinks are involved, and the arguments are
 * variables, it performs alpha conversion instead of beta reduction.
 * (as that's kind-of "the same thing", for variables).
 *
 * When multiple arguments are presented as a  set, then the put
 * is applied to each member of the set; the result is a set.
 *
 * When the set is a singleton, that singleton is unwrapped, and
 * the set is discarded. This is done in order to play nice with
 * GetLink, which always returns SetLinks, sometimes uneccesarily.
 *
 * Users who need a more complete apply-like environment should
 * look to the EvaluationLink, the ExecutionOutputLink, or the
 * Instantiator.
 *
 * So, for example, if the PutLink looks like this:
 *
 *   PutLink
 *      EvaluationLink
 *         PredicateNode "is a kind of"
 *         ListLink
 *            VariableNode $a
 *            ConceptNode "hot patootie"
 *      ConceptNode "cowpie"
 *
 * then the reduced body will be
 *
 *   EvaluationLink
 *      PredicateNode "is a kind of"
 *      ListLink
 *         ConceptNode "cowpie"
 *         ConceptNode "hot patootie"
 *
 * Type checking is performed during substitution; if the arguments fail
 * to have the desired types, no substitution is performed.  In this case,
 * an undefined handle is returned (?? XXX really? or is it a throw?).
 * For set substitutions, this acts as a filter, removing (filtering out)
 * the mismatched types.
 *
 * Note that the resulting tree is NOT placed into any atomspace!
 */
Handle PutLink::do_reduce(void) const
{
	Handle bods(_body);
	Variables vars(_varlist);
	PrenexLinkPtr subs(PrenexLinkCast(get_handle()));
	Handle args(_arguments);

	if (args->is_executable())
		args = HandleCast(args->execute());

	// Resolve the body, if needed. That is, if the body is
	// given in a defintion, get that defintion.
	Type btype = _body->get_type();
	if (DEFINED_SCHEMA_NODE == btype or
	    DEFINED_PREDICATE_NODE == btype)
	{
		bods = DefineLink::get_definition(bods);
		btype = bods->get_type();
		// XXX TODO we should perform a type-check on the function.
		if (not nameserver().isA(btype, LAMBDA_LINK))
			throw InvalidParamException(TRACE_INFO,
					"Expecting a LambdaLink, got %s",
			      bods->to_string().c_str());
	}

	// If the body is itself a PutLink, then reduce it first
	if (PUT_LINK == btype)
	{
		PutLinkPtr nested_put = PutLinkCast(bods);
		nested_put->make_silent(_silent);
		bods = nested_put->do_reduce();
		btype = bods->get_type();
	}

	// If the body is a lambda, work with that.
	if (nameserver().isA(btype, LAMBDA_LINK))
	{
		LambdaLinkPtr lam(LambdaLinkCast(bods));
		bods = lam->get_body();
		vars = lam->get_variables();
		btype = bods->get_type();
		subs = lam;
	}

	// Now get the arguments that we will plug into the body.
	Type vtype = args->get_type();
	size_t nvars = vars.varseq.size();

	// FunctionLinks behave like pointless lambdas; that is, one can
	// create valid beta-redexes with them. We handle that here.
	//
	// At this time, we don't know the number of arguments any given
	// FunctionLink might take.  Atomese does have the mechanisms
	// to declare these, including arbitrary-arity functions, its
	// just that it is currently not declared anywhere for any of the
	// FunctionLinks.  So we just guess as best we can.  Example usage:
	// (cog-execute! (Put (Plus) (List (Number 2) (Number 2))))
	// (cog-execute! (Put (Plus (Number 9)) (List (Number 2) (Number 2))))
	if (0 == nvars and nameserver().isA(btype, FUNCTION_LINK))
	{
		if (LIST_LINK == vtype)
		{
			HandleSeq oset(bods->getOutgoingSet());
			for (const Handle& arg : args->getOutgoingSet())
				oset.emplace_back(expand(arg, _silent));
			return createLink(oset, btype);
		}

		if (SET_LINK != vtype)
		{
			HandleSeq oset(bods->getOutgoingSet());
			oset.emplace_back(args);
			return createLink(oset, btype);
		}

		// If the arguments are given in a set, then iterate over the set...
		HandleSeq bset;
		for (const Handle& h : args->getOutgoingSet())
		{
			if (LIST_LINK == h->get_type())
			{
				HandleSeq oset(bods->getOutgoingSet());
				for (const Handle& arg : h->getOutgoingSet())
					oset.emplace_back(expand(arg, _silent));
				bset.emplace_back(createLink(oset, btype));
			}
			else
			{
				HandleSeq oset(bods->getOutgoingSet());
				oset.emplace_back(expand(h, _silent));
				bset.emplace_back(createLink(oset, btype));
			}
		}
		return createLink(bset, SET_LINK);
	}

	// If there is only one variable in the PutLink body...
	if (1 == nvars)
	{

		if (SET_LINK == vtype)
		{
			// If the arguments are given in a set, then iterate over the set...
			HandleSeq bset;
			for (const Handle& h : args->getOutgoingSet())
			{
				HandleSeq oset;
				oset.emplace_back(h);
				try
				{
					bset.emplace_back(reddy(subs, oset));
				}
				catch (const TypeCheckException& ex) {}
			}
			return createLink(bset, SET_LINK);
		}

		if (SET_NODE == vtype)
		{
			AtomSpace* as = getAtomSpace();
			for (const LinkPtr& lp : args->getIncomingSetByType(MEMBER_LINK))
			{
				Handle h = lp->getOutgoingAtom(0);
				if (as) as->add_atom(reddy(subs, {h}));
			}
			return args;
		}

		return reddy(subs, {args});
	}

	// If we are here, then there are multiple variables in the body.
	// See how many arguments there are.  If the arguments are in a
	// ListLink, then assume that there is only a single set of
	// arguments to plug in.
	if (LIST_LINK == vtype)
	{
		HandleSeq oset;
		for (const Handle& h: args->getOutgoingSet())
			oset.push_back(expand(h, _silent));
		return reddy(subs, oset);
	}

	// If the argument is a LambdaLink, it will eta-reducible.
	// We already checked this earlier (a static check), so we
	// don't need any more checking. Just pass it through.
	if (LAMBDA_LINK == vtype)
	{
		HandleSeq oset;
		oset.emplace_back(args);
		return reddy(subs, oset);
	}

	// If we are here, then there are multiple arguments.
	// These MUST be given to us as a SetLink.
	if (SET_LINK != vtype)
	{
		if (_silent)
			throw TypeCheckException();

		throw RuntimeException(TRACE_INFO,
		                       "Should have caught this earlier, in the ctor");
	}

	HandleSeq bset;
	for (const Handle& h : args->getOutgoingSet())
	{
		const HandleSeq& oset = h->getOutgoingSet();
		try
		{
			bset.emplace_back(reddy(subs, oset));
		}
		catch (const TypeCheckException& ex) {}
	}
	return createLink(bset, SET_LINK);
}

ValuePtr PutLink::execute(AtomSpace* as, bool silent)
{
	_silent = silent;
	return do_reduce();
}

DEFINE_LINK_FACTORY(PutLink, PUT_LINK)

/* ===================== END OF FILE ===================== */
