/*
 * Instantiator.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/core/PutLink.h>
#include <opencog/atoms/execution/ExecutionOutputLink.h>
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/reduct/FoldLink.h>
#include <opencog/query/BindLinkAPI.h>

#include "Instantiator.h"

using namespace opencog;

bool Instantiator::walk_tree(HandleSeq& oset_results, const HandleSeq& expr)
{
	bool changed = false;
	for (const Handle& h : expr)
	{
		Handle hg(walk_tree(h));
		if (hg != h) changed = true;

		// GlobNodes are grounded by a ListLink of everything that
		// the GlobNode matches. Unwrap the list, and insert each
		// of the glob elements in sequence.
		if (GLOB_NODE == h->getType() and hg != h)
		{
			LinkPtr lp(LinkCast(hg));
			OC_ASSERT(nullptr != lp, "Expecting glob list");
			for (const Handle& gloe: lp->getOutgoingSet())
			{
				if (NULL != gloe)
					oset_results.emplace_back(gloe);
			}
		}
		else
		{
			// It could be a NULL handle if it's deleted... Just skip
			// over it. We test the pointer here, not the uuid, since
			// the uuid's are all Handle::UNDEFINED until we put them
			// into the atomspace.
			if (NULL != hg)
				oset_results.emplace_back(hg);
		}
	}
	return changed;
}

Handle Instantiator::walk_tree(const Handle& expr)
{
	Type t = expr->getType();

	// Must not explore the insides of a QuoteLink.
	// XXX TODO: Need to implement UNQUOTE_LINK here...
	if (QUOTE_LINK == t)
		return Handle(expr);

	LinkPtr lexpr(LinkCast(expr));
	if (not lexpr)
	{
		// If we are here, we are a Node.
		if (DEFINED_SCHEMA_NODE == t)
		{
			return walk_tree(DefineLink::get_definition(expr));
		}

		if (VARIABLE_NODE != t and GLOB_NODE != t)
			return Handle(expr);

		// If we are here, we found a variable. Look it up. Return a
		// grounding if it has one, otherwise return the variable
		// itself.
		std::map<Handle,Handle>::const_iterator it = _vmap->find(expr);
		if (_vmap->end() == it) return Handle(expr);

		// Not so fast, pardner. VariableNodes can be grounded by
		// links, and those links may be executable. In that case,
		// we have to execute them.

		// halt infinite regress
		if (_halt)
			return Handle(expr);

		_halt = true;
		Handle hgnd(walk_tree(it->second));
		_halt = false;
		return hgnd;
	}

	// -----------------------------------------------------------
	// If we are here, then we have a link. Walk it. In general,
	// links may contain both bound variables, and also free variables.
	// We must be careful to substitute only for free variables, and
	// never for bound ones.
	//
	// Reduce PutLinks.
	if (PUT_LINK == t)
	{
		PutLinkPtr ppp(PutLinkCast(expr));
		if (nullptr == ppp)
			ppp = createPutLink(*lexpr);

		// Execute the values in the PutLink before ding the beta-reduction.
		// Execute the body only after the beta-reduction has been done.
		Handle pvals = ppp->get_values();
		Handle gargs = walk_tree(pvals);
		if (gargs != pvals)
		{
			HandleSeq groset;
			if (ppp->get_vardecl())
				groset.emplace_back(ppp->get_vardecl());
			groset.emplace_back(ppp->get_body());
			groset.emplace_back(gargs);
			ppp = createPutLink(groset);
		}
		// Step one: beta-reduce.
		Handle red(ppp->reduce());
		// Step two: execute the resulting body.
		Handle rex(walk_tree(red));
		if (nullptr == rex)
			return rex;

		// Step three: XXX this is awkward, but seems to be needed...
		// If the result is evaluatable, then evaluate it. e.g. if the
		// result has a GroundedPredicateNode, we need to run it now.
		// We do, however, ignore the reulsting TV, which is also
		// awkward.  I'm confused about how to handle this best.
		// The behavior tree uses this!
		// Anyway, do_evaluate() will throw if rex is not evaluatable.
		if (SET_LINK == rex->getType())
		{
			LinkPtr slp(LinkCast(rex));
			for (const Handle& plo : slp->getOutgoingSet())
			{
				try {
					EvaluationLink::do_evaluate(_as, plo);
				}
				catch (...) {}
			}
			return rex;
		}
		try {
			EvaluationLink::do_evaluate(_as, rex);
		}
		catch (...) {}
		return rex;
	}

	// ExecutionOutputLinks are not handled by the FunctionLink factory
	// below. This is due to a circular shared-libarary dependency.
	if (EXECUTION_OUTPUT_LINK == t)
	{
		// At this time, the GSN or the DSN is always in position 0
		// of the outgoing set, and the ListLink of arguments is always
		// in position 1.  Someday in the future, there may be a variable
		// declaration; we punt on that.
		Handle sn(lexpr->getOutgoingAtom(0));
		Handle args(lexpr->getOutgoingAtom(1));

		// Perform substitution on the args, only.
		args = walk_tree(args);

		// If its a DSN, obtain the correct body for it.
		if (DEFINED_SCHEMA_NODE == sn->getType())
			sn = DefineLink::get_definition(sn);

		// If its an anonymous function link, execute it here.
		if (LAMBDA_LINK == sn->getType())
		{
			LambdaLinkPtr flp(LambdaLinkCast(sn));
			if (NULL == flp)
				flp = createLambdaLink(*LinkCast(sn));

			// Two-step process. First, plug the arguments into the
			// function; i.e. perform beta-reduction. Second, actually
			// execute the result. We execute by just calling walk_tree
			// again.
			Handle body(flp->get_body());
			Variables vars(flp->get_variables());

			const HandleSeq& oset(LinkCast(args)->getOutgoingSet());
			Handle beta_reduced(vars.substitute_nocheck(body, oset));
			return walk_tree(beta_reduced);
		}

		ExecutionOutputLinkPtr eolp(createExecutionOutputLink(sn, args));
		return eolp->execute(_as);
	}

	// FoldLink's cannot be handled by the factory below, due to
	// ciruclar shared library dependencies. Yuck. Something better
	// than a factory needs to be invented.
	if (classserver().isA(t, FOLD_LINK))
	{
		// At this time, no FoldLink ever has a variable declaration,
		// and the number of arguments is not fixed, i.e. variadic.
		// Perform substitution on all arguments before applying the
		// function itself.
		HandleSeq oset_results;
		walk_tree(oset_results, lexpr->getOutgoingSet());
		Handle hl(FoldLink::factory(t, oset_results));
		FoldLinkPtr flp(FoldLinkCast(hl));
		return flp->execute(_as);
	}

	// Handle DeleteLink's before general FunctionLink's; they
	// work differently.
	if (DELETE_LINK == t)
	{
		HandleSeq oset_results;
		walk_tree(oset_results, lexpr->getOutgoingSet());
		for (const Handle& h: oset_results)
		{
			Type ht = h->getType();
			if (VARIABLE_NODE != ht and GLOB_NODE != ht)
				_as->remove_atom(h, true);
		}
		return Handle::UNDEFINED;
	}

	// Fire any other function links, not handled above.
	if (classserver().isA(t, FUNCTION_LINK))
	{
		// At this time, no FunctionLink that is outsode of an
		// ExecutionOutputLink ever has a variable declaration.
		// Also, the number of arguments is not fixed, i.e. variadic.
		// Perform substitution on all arguments before applying the
		// function itself.
		HandleSeq oset_results;
		walk_tree(oset_results, lexpr->getOutgoingSet());
		Handle hl(FunctionLink::factory(t, oset_results));
		FunctionLinkPtr flp(FunctionLinkCast(hl));
		return flp->execute(_as);
	}

	// If there is a GetLink, we have to perform the get, and replace
	// it with the results of the get. The get is implemented with the
	// PatternLink::satisfy() method.
	if (GET_LINK == t)
	{
		HandleSeq oset_results;
		walk_tree(oset_results, lexpr->getOutgoingSet());
		size_t sz = oset_results.size();
		for (size_t i=0; i< sz; i++)
			oset_results[i] = _as->add_atom(oset_results[i]);

		LinkPtr lp(createLink(GET_LINK, oset_results));

		return satisfying_set(_as, Handle(lp));
	}

	// None of the above. Create a duplicate link, but with an outgoing
	// set where the variables have been substituted by their values.
	HandleSeq oset_results;
	bool changed = walk_tree(oset_results, lexpr->getOutgoingSet());
	if (changed)
		return Handle(createLink(t, oset_results, expr->getTruthValue()));
	return expr;
}

/**
 * instantiate -- create a grounded expression from an ungrounded one.
 *
 * Given a handle to an ungrounded expression, and a set of groundings,
 * this will create a grounded expression.
 *
 * The set of groundings is to be passed in with the map 'vars', which
 * maps variable names to their groundings -- it maps variable names to
 * atoms that already exist in the atomspace.  This method will then go
 * through all of the variables in the expression, and substitute them
 * with their values, creating a new expression. The new expression is
 * added to the atomspace, and its handle is returned.
 */
Handle Instantiator::instantiate(const Handle& expr,
                                 const std::map<Handle, Handle> &vars)
{
	// throw, not assert, because this is a user error ...
	if (nullptr == expr)
		throw InvalidParamException(TRACE_INFO,
			"Asked to ground a null expression");

	_vmap = &vars;

	// The returned handle is not yet in the atomspace. Add it now.
	// We do this here, instead of in walk_tree(), because adding
	// atoms to the atomspace is an expensive process.  We can save
	// some time by doing it just once, right here, in one big batch.
	return _as->add_atom(walk_tree(expr));
}

/* ===================== END OF FILE ===================== */
