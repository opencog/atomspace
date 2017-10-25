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


/// Perform beta-reduction on the expression `expr`, using the `vmap`
/// to fish out values for variables.  The map holds pairs: the first
/// member of the pair is the variable; the second is the value that
/// should be used as its replacement.  (Note that "variables" do not
/// have to actually be VariableNode's; they can be any atom.)
static Handle beta_reduce(const Handle& expr, const HandleMap vmap)
{
	// XXX crud.  Stupid inefficient format conversion. FIXME.
	// FreeVariables::substitute_nocheck() performs beta-reduction
	// correctly, so we just use that. But it takes a specific
	// format, and a variable-value map is not one of them.
	HandleSeq vals;
	FreeVariables crud;
	unsigned int idx = 0;
	for (const auto& pr : vmap)
	{
		crud.varseq.push_back(pr.first);
		crud.index.insert({pr.first, idx});
		vals.push_back(pr.second);
		idx++;
	}
	return crud.substitute_nocheck(expr, vals);
}

/// Same as walk tree, except that it handles a handle sequence,
/// instead of a single handle. The returned result is in oset_results.
/// Returns true if the results differ from the input, i.e. if the
/// result of execution/evaluation changed something.
bool Instantiator::walk_sequence(HandleSeq& oset_results,
                                 const HandleSeq& expr,
                                 bool silent)
{
	bool changed = false;
	Context cp_context = _context;
	for (const Handle& h : expr)
	{
		Handle hg(walk_tree(h, silent));
		_context = cp_context;
		if (hg != h) changed = true;

		// GlobNodes are grounded by a ListLink of everything that
		// the GlobNode matches. Unwrap the list, and insert each
		// of the glob elements in sequence.
		if (_context.is_unquoted() and GLOB_NODE == h->get_type() and hg != h)
		{
			for (const Handle& gloe: hg->getOutgoingSet())
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

Handle Instantiator::walk_tree(const Handle& expr, bool silent)
{
	Type t = expr->get_type();

	// Store the current context so we can update it for subsequent
	// recursive calls of walk_tree.
	Context context_cp(_context);
	_context.update(expr);

	// Discard the following QuoteLink, UnquoteLink or LocalQuoteLink
	// as it is serving its quoting or unquoting function.
	if (_avoid_discarding_quotes_level == 0 and context_cp.consumable(t))
	{
		if (1 != expr->get_arity())
			throw InvalidParamException(TRACE_INFO,
			                            "QuoteLink/UnquoteLink has "
			                            "unexpected arity!");
		return walk_tree(expr->getOutgoingAtom(0), silent);
	}

	if (expr->is_node())
	{
		if (context_cp.is_quoted())
			return expr;

		// If we are here, we are a Node.
		if (DEFINED_SCHEMA_NODE == t)
		{
			return walk_tree(DefineLink::get_definition(expr), silent);
		}

		if (VARIABLE_NODE != t and GLOB_NODE != t)
			return expr;

		// If it is a quoted or shadowed variable don't substitute.
		// TODO: what about globs?
		if (VARIABLE_NODE == t and not context_cp.is_free_variable(expr))
			return expr;

		// If we are here, we found a free variable (or glob?). Look
		// it up. Return a grounding if it has one, otherwise return
		// the variable itself.
		HandleMap::const_iterator it = _vmap->find(expr);
		if (_vmap->end() == it) return expr;

		// Not so fast, pardner. VariableNodes can be grounded by
		// links, and those links may be executable. In that case,
		// we have to execute them.

		// halt infinite regress
		if (_halt)
			return expr;

		_halt = true;
		Handle hgnd(walk_tree(it->second, silent));
		_halt = false;
		return hgnd;
	}

	// -----------------------------------------------------------
	// If we are here, then we have a link. Walk it. In general,
	// links may contain both bound variables, and also free variables.
	// We must be careful to substitute only for free variables, and
	// never for bound ones.

	if (context_cp.is_quoted())
		goto mere_recursive_call;

	// Reduce PutLinks. There are two ways to do this: eager execution
	// and lazy execution.  The algos are this:
	//
	//    Eager: first, execute the arguments to the Put, then beta-
	//    reduce, then execute again.
	//
	//    Lazy: beta-reduce first, then execute.  Lazy helps avoid
	//    un-needed executions, and has better control over infinite
	//    recursion. However, unit tests currently fail on it.
	//
	if (PUT_LINK == t)
	{
		PutLinkPtr ppp;

		if (_eager)
		{
			ppp = PutLinkCast(expr);
			// Execute the values in the PutLink before doing the
			// beta-reduction. Execute the body only after the
			// beta-reduction has been done.
			Handle pvals = ppp->get_values();
			Handle gargs = walk_tree(pvals, silent);
			if (gargs != pvals)
			{
				HandleSeq groset;
				if (ppp->get_vardecl())
					groset.emplace_back(ppp->get_vardecl());
				groset.emplace_back(ppp->get_body());
				groset.emplace_back(gargs);
				ppp = createPutLink(groset);
			}
		}
		else
		{
			Handle hexpr(beta_reduce(expr, *_vmap));
			ppp = PutLinkCast(hexpr);
		}

		// Step one: beta-reduce.
		Handle red(ppp->reduce());

		// Step two: execute the resulting body.
		// (unless its not executable)
		if (DONT_EXEC_LINK == red->get_type())
			return red->getOutgoingAtom(0);

		Handle rex(walk_tree(red, silent));
		if (nullptr == rex)
			return rex;

		// Step three: XXX this is awkward, but seems to be needed...
		// If the result is evaluatable, then evaluate it. e.g. if the
		// result has a GroundedPredicateNode, we need to run it now.
		// We do, however, ignore the resulting TV, which is also
		// awkward.  I'm confused about how to handle this best.
		// The behavior tree uses this!
		// Anyway, do_evaluate() will throw if rex is not evaluatable.
		if (SET_LINK == rex->get_type())
		{
			for (const Handle& plo : rex->getOutgoingSet())
			{
				try {
					EvaluationLink::do_evaluate(_as, plo, true);
				}
				catch (const NotEvaluatableException& ex) {}
			}
			return rex;
		}
		try {
			EvaluationLink::do_evaluate(_as, rex, true);
		}
		catch (const NotEvaluatableException& ex) {}
		return rex;
	}

	// ExecutionOutputLinks are not handled by the FunctionLink factory
	// below. This is due to a circular shared-libarary dependency.
	//
	// Even for the case of lazy execution, we still have to do eager
	// execution of the arguments passed to the ExOutLink.  This is
	// because the ExOutLink is a black box, and we cannot guess what
	// it might do.  It would be great if the authors of ExOutLinks
	// did the lazy execution themselves... but this is too much to
	// ask for. So we always eager-evaluate those args.
	if (EXECUTION_OUTPUT_LINK == t)
	{
		ExecutionOutputLinkPtr eolp(ExecutionOutputLinkCast(expr));

		// At this time, the GSN or the DSN is always in position 0
		// of the outgoing set, and the ListLink of arguments is always
		// in position 1.  Someday in the future, there may be a variable
		// declaration; we punt on that.
		Handle sn(eolp->get_schema());
		Handle args(eolp->get_args());

		// If its a DSN, obtain the correct body for it.
		if (DEFINED_SCHEMA_NODE == sn->get_type())
			sn = DefineLink::get_definition(sn);

		// If its an anonymous function link, execute it here.
		if (LAMBDA_LINK == sn->get_type())
		{
			LambdaLinkPtr flp(LambdaLinkCast(sn));

			// Two-step process. First, plug the arguments into the
			// function; i.e. perform beta-reduction. Second, actually
			// execute the result. We execute by just calling walk_tree
			// again.
			Handle body(flp->get_body());
			Variables vars(flp->get_variables());

			// Perform substitution on the args, only.
			if (_eager)
			{
				_avoid_discarding_quotes_level++;
				args = walk_tree(args, silent);
				_avoid_discarding_quotes_level--;
			}
			else
			{
				args = beta_reduce(args, *_vmap);
			}

			const HandleSeq& oset(args->getOutgoingSet());
			Handle beta_reduced(vars.substitute_nocheck(body, oset));
			return walk_tree(beta_reduced, silent);
		}

		// Perform substitution on the args, only.
		if (_eager)
		{
			// XXX I don't get it ... something is broken here, because
			// the ExecutionOutputLink below *also* performs eager
			// execution of its arguments. So the step below should not
			// be needed -- yet, it is ... Funny thing is, it only
			// breaks the BackwardChainerUTest ... why?
			_avoid_discarding_quotes_level++;
			args = walk_tree(args, silent);
			_avoid_discarding_quotes_level--;
		}
		else
		{
			args = beta_reduce(args, *_vmap);
		}

		ExecutionOutputLinkPtr geolp(createExecutionOutputLink(sn, args));
		return geolp->execute(_as, silent);
	}

	// Handle DeleteLink's before general FunctionLink's; they
	// work differently.
	if (DELETE_LINK == t)
	{
		HandleSeq oset_results;
		walk_sequence(oset_results, expr->getOutgoingSet(), silent);
		for (const Handle& h: oset_results)
		{
			Type ht = h->get_type();
			if (VARIABLE_NODE != ht and GLOB_NODE != ht)
				_as->remove_atom(h, true);
		}
		return Handle::UNDEFINED;
	}

	// FoldLink's are a kind-of FunctionLink, but are not currently
	// handled by the FunctionLink factory below.  This should be fixed
	// someday, when the reduct directory is nuked.
	if (classserver().isA(t, FOLD_LINK))
	{
		// A FoldLink never has a variable declaration (at this time).
		// The number of arguments is never fixed, its always variadic.
		if (_eager)
		{
			// Perform substitution on all arguments before applying the
			// function itself.
			HandleSeq oset_results;
			walk_sequence(oset_results, expr->getOutgoingSet(), silent);
			Handle fh(createLink(oset_results, t));
			FoldLinkPtr flp(FoldLinkCast(fh));
			return flp->execute(_as);
		}
		else
		{
			Handle hexpr(beta_reduce(expr, *_vmap));
			FoldLinkPtr flp(FoldLinkCast(hexpr));
			return flp->execute(_as);
		}
	}

	// Fire any other function links, not handled above.
	if (classserver().isA(t, FUNCTION_LINK))
	{
		if (_eager)
		{
			// Perform substitution on all arguments before applying the
			// function itself. XXX FIXME -- We can almost but not quite
			// avoid eager execution here ... however, many links, e.g.
			// the RandomChoiceLink will typically take a GetLink as an
			// argument, and, due to the stupid, fucked-up CMake
			// shared-library dependency problem, we cannot get
			// FunctionLinks to perform thier own evaluation of arguments.
			// So we have to do eager evaluation, here.  This stinks, and
			// needs fixing.
			HandleSeq oset_results;
			walk_sequence(oset_results, expr->getOutgoingSet(), silent);

			FunctionLinkPtr flp(FunctionLinkCast(createLink(oset_results, t)));
			return flp->execute(_as);
		}
		else
		{
			// At this time, no FunctionLink that is outside of an
			// ExecutionOutputLink ever has a variable declaration.
			// Also, the number of arguments is not fixed, its always variadic.
			// Perform substitution on all arguments before applying the
			// function itself.
			FunctionLinkPtr flp(FunctionLinkCast(expr));
			return flp->execute(_as);
		}
	}

	// If there is a SatisfyingLink, we have to perform it
	// and return the saisfying set.
	if (classserver().isA(t, SATISFYING_LINK))
	{
		return satisfying_set(_as, expr);
	}

	// Ideally, we should not evaluate any EvaluatableLinks.
	// However, some of these may hold embedded executable links
	// inside of them, which the current unit tests and code
	// expect to be executed.  Thus, for right now, we only avoid
	// evaluating VirtualLinks, as these all are capable of thier
	// own lazy-evaluation, and so, if evaluation is needed,
	// it will be triggered by something else.
	// Non-virtual evaluatables fall through and are handled
	// below.
	// if (classserver().isA(t, EVALUATABLE_LINK))
	if (classserver().isA(t, VIRTUAL_LINK))
	{
		if (_vmap->empty()) return expr;
		return beta_reduce(expr, *_vmap);
	}

	// If an atom is wrapped by the DontExecLink, then unwrap it,
	// beta-reduce it, but don't execute it. Consume the DontExecLink.
	// Actually, don't consume it. See discussion at issue #1303.
	// XXX FIXME -- not consuming it seems wrong; this needs more
	// analysis and experimentation.
	if (DONT_EXEC_LINK == t)
	{
#ifdef CONSUME_THE_EXEC
		if (_vmap->empty()) return expr->getOutgoingAtom(0);
		return beta_reduce(expr->getOutgoingAtom(0), *_vmap);
#else
		if (_vmap->empty()) return expr;
		return beta_reduce(expr, *_vmap);
#endif
	}

	// None of the above. Create a duplicate link, but with an outgoing
	// set where the variables have been substituted by their values.
mere_recursive_call:
	HandleSeq oset_results;
	bool changed = walk_sequence(oset_results, expr->getOutgoingSet(), silent);
	if (changed)
	{
		Handle subl(createLink(oset_results, t));
		subl->copyValues(expr);
		return _as->add_atom(subl);
	}
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
                                 const HandleMap &vars,
                                 bool silent)
{
	// throw, not assert, because this is a user error ...
	if (nullptr == expr)
		throw InvalidParamException(TRACE_INFO,
			"Asked to ground a null expression");

	_context = Context(false);
	_avoid_discarding_quotes_level = 0;

	_vmap = &vars;

	// The returned handle is not yet in the atomspace. Add it now.
	// We do this here, instead of in walk_tree(), because adding
	// atoms to the atomspace is an expensive process.  We can save
	// some time by doing it just once, right here, in one big batch.
	return _as->add_atom(walk_tree(expr, silent));
}

/* ===================== END OF FILE ===================== */
