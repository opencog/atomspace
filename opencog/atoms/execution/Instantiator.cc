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

#include "Instantiator.h"

using namespace opencog;

Instantiator::Instantiator(AtomSpace* as)
	: _as(as), _vmap(nullptr), _halt(false),
	  _consume_quotations(true),
	  _needless_quotation(true),
	  _eager(true) {}

/// Perform beta-reduction on the expression `expr`, using the `vmap`
/// to fish out values for variables.  The map holds pairs: the first
/// member of the pair is the variable; the second is the value that
/// should be used as its replacement.  (Note that "variables" do not
/// have to actually be VariableNode's; they can be any atom.)
static Handle beta_reduce(const Handle& expr, const HandleMap& vmap)
{
	// Format conversion. FreeVariables::substitute_nocheck() performs
	// beta-reduction correctly, so we just use that. But we have to
	// jam the map into the format it expects.
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
			// It could be a NULL handle if it's deleted...
			// Just skip over it.
			if (hg) oset_results.emplace_back(hg);
		}
	}
	return changed;
}

// ExecutionOutputLinks get special treatment.
//
// Even for the case of lazy execution, we still have to do eager
// execution of the arguments passed to the ExOutLink.  This is
// because the ExOutLink is a black box, and we cannot guess what
// it might do.  It would be great if the authors of ExOutLinks
// did the lazy execution themselves... but this is too much to
// ask for. So we always eager-evaluate those args.
Handle Instantiator::reduce_exout(const Handle& expr, bool silent)
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
			args = walk_tree(args, silent);
		}
		else
		{
			args = beta_reduce(args, *_vmap);
		}
		const Type& arg_type = args->get_type();
		// unpack list link
		const HandleSeq& oset(arg_type == LIST_LINK ? args->getOutgoingSet():
		                                              HandleSeq{args});
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
		args = walk_tree(args, silent);
	}
	else
	{
		args = beta_reduce(args, *_vmap);
	}

	Type t = expr->get_type();
	return createLink(t, sn, args);
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
	if ((_consume_quotations or _needless_quotation) and
	    context_cp.consumable(t))
	{
		if (1 != expr->get_arity())
			throw InvalidParamException(TRACE_INFO,
			                            "QuoteLink/UnquoteLink has "
			                            "unexpected arity!");
		Handle child = expr->getOutgoingAtom(0);
		Handle walked_child = walk_tree(child, silent);

		// Only consume if the quotation is really needless (walking
		// the children might have changed _needless_quotation).
		if (_consume_quotations or _needless_quotation)
			return walked_child;

		// Otherwise keep the quotation, but set _needless_quotation
		// back to true for the remaining tree
		_needless_quotation = true;
		Handle nexp(createLink(t, walked_child));
		nexp->copyValues(expr);
		return nexp;
	}

	if (expr->is_node())
	{
		if (context_cp.is_quoted())
		{
			// Make sure we don't consume a useful quotation
			if (not_self_match(t))
				_needless_quotation = false;

			return expr;
		}

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
	{
		// Make sure we don't consume a useful quotation
		if (not_self_match(t))
			_needless_quotation = false;
		goto mere_recursive_call;
	}

	// Reduce PutLinks. There are two ways to do this: eager execution
	// and lazy execution.  The algos are this:
	//
	//    Eager: first, execute the arguments to the Put, then beta-
	//    reduce, then execute again.
	//
	//    Lazy: beta-reduce first, then execute.  Lazy can sometimes
	//    avoid un-needed executions, although it can sometimes lead to
	//    more of them. Lazy has better control over infinite recursion.
	//
	if (PUT_LINK == t)
	{
		PutLinkPtr ppp;

		if (_eager)
		{
			ppp = PutLinkCast(expr);
			// Execute the arguments in the PutLink before doing
			// the beta-reduction. Execute the PutLink only after
			// the beta-reduction has been done.
			Handle pargs = ppp->get_arguments();
			Handle gargs = walk_tree(pargs, silent);
			if (gargs != pargs)
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
		ppp->make_silent(silent);
		Handle red(ppp->reduce());

		if (nullptr == red)
			return red;

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
		// Anyway, do_evaluate() will throw if rex is not evaluatable.
		//
		// The DontExecLink is a weird hack to halt evaluation.
		// We unwrap it and throw it away when encountered.
		// Some long-term fix is needed that avoids this step-three
		// entirely.
		if (SET_LINK == rex->get_type())
		{
			HandleSeq unwrap;
			for (const Handle& plo : rex->getOutgoingSet())
			{
				if (DONT_EXEC_LINK == plo->get_type())
				{
					unwrap.push_back(plo->getOutgoingAtom(0));
				}
				else
				{
					try {
						TruthValuePtr tvp =
							EvaluationLink::do_evaluate(_as, plo, true);
						plo->setTruthValue(tvp);
					}
					catch (const NotEvaluatableException& ex) {}
					unwrap.push_back(plo);
				}
			}
			return createLink(unwrap, SET_LINK);
		}

		try {
			EvaluationLink::do_evaluate(_as, rex, true);
		}
		catch (const NotEvaluatableException& ex) {}
		return rex;
	}

	// LambdaLink may get special treatment in case it is used for
	// pattern matching. For instance if a connector is quoted, we
	// don't want to consume that quote otherwise the connector will
	// serve as a logic connector to the pattern matcher instead of
	// serving as self-match.
	if (LAMBDA_LINK == t)
	{
		LambdaLinkPtr ll = LambdaLinkCast(expr);
		Handle vardecl = ll->get_vardecl();

		// Recursively walk vardecl
		if (vardecl)
			vardecl = walk_tree(vardecl, silent);

		// Recursively walk body, making sure quotation is preserved
		Handle body = ll->get_body();
		// If the lambda is ill-formed it might not have a body, throw
		// an exception then
		if (not body)
		{
			if (silent)
				throw NotEvaluatableException();
			throw SyntaxException(TRACE_INFO, "body is ill-formed");
		}
		Type bt = body->get_type();
		if (Quotation::is_quotation_type(bt))
		{
			_context.update(body);
			_needless_quotation = false;
			body = walk_tree(body->getOutgoingAtom(0), silent);
			body = createLink(bt, body);
			_needless_quotation = true;
		} else
		{
			body = walk_tree(body, silent);
		}
		// Reconstruct Lambda, if it has changed
		if (ll->get_vardecl() != vardecl or ll->get_body() != body)
		{
			HandleSeq oset{body};
			if (vardecl)
				oset.insert(oset.begin(), vardecl);
			// TODO: copy values
			return createLink(oset, LAMBDA_LINK);
		}
		return expr;
	}

	// ExecutionOutputLinks
	if (nameserver().isA(t, EXECUTION_OUTPUT_LINK))
	{
		Handle eolh = reduce_exout(expr, silent);
		t = eolh->get_type();
		if (nameserver().isA(t, EXECUTION_OUTPUT_LINK))
		{
			ExecutionOutputLinkPtr geolp(ExecutionOutputLinkCast(eolh));
			return HandleCast(geolp->execute(_as, silent));
		}
		return eolh;
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

	// Fire any other function links, not handled above.
	if (nameserver().isA(t, FUNCTION_LINK))
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

			Handle flp(createLink(oset_results, t));
			return HandleCast(flp->execute(_as, silent));
		}
		else
		{
			// At this time, no FunctionLink that is outside of an
			// ExecutionOutputLink ever has a variable declaration.
			// Also, the number of arguments is not fixed, its always variadic.
			// Perform substitution on all arguments before applying the
			// function itself.
			return HandleCast(expr->execute(_as, silent));
		}
	}

	// If there is a SatisfyingLink, we have to perform it
	// and return the satisfying set.
	if (nameserver().isA(t, SATISFYING_LINK))
	{
		return HandleCast(expr->execute(_as, silent));
	}

	// Ideally, we should not evaluate any EvaluatableLinks.
	// However, some of these may hold embedded executable links
	// inside of them, which the current unit tests and code
	// expect to be executed.  Thus, for right now, we only avoid
	// evaluating VirtualLinks, as these all are capable of their
	// own lazy-evaluation, and so, if evaluation is needed,
	// it will be triggered by something else.
	// Non-virtual evaluatables fall through and are handled
	// below.
	// if (nameserver().isA(t, EVALUATABLE_LINK))
	if (nameserver().isA(t, VIRTUAL_LINK))
	{
		if (_vmap->empty()) return expr;
		return beta_reduce(expr, *_vmap);
	}

	// Do not reduce PredicateFormulaLink. That is because it contains
	// formulas that we will need to re-evaluate in the future, so we
	// must not clobber them.
	if (PREDICATE_FORMULA_LINK == t)
	{
		return expr;
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
		return subl;
	}
	return expr;
}

bool Instantiator::not_self_match(Type t)
{
	return nameserver().isA(t, SCOPE_LINK) or
		nameserver().isA(t, FUNCTION_LINK) or
		nameserver().isA(t, DELETE_LINK) or
		nameserver().isA(t, VIRTUAL_LINK) or
		nameserver().isA(t, DEFINE_LINK) or
		nameserver().isA(t, DEFINED_SCHEMA_NODE) or
		nameserver().isA(t, DEFINED_PREDICATE_NODE) or
		nameserver().isA(t, DONT_EXEC_LINK);
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
ValuePtr Instantiator::instantiate(const Handle& expr,
                                   const HandleMap &vars,
                                   bool silent)
{
	// throw, not assert, because this is a user error ...
	if (nullptr == expr)
		throw InvalidParamException(TRACE_INFO,
			"Asked to ground a null expression");

	_context = Context(false);
	_needless_quotation = true;

	_vmap = &vars;

	// Most of the work happens in walk_tree (which returns a Handle
	// to the instantiated tree). However, special-case the handling
	// of expr being a FunctionLink - this can return a Value, which
	// walk_tree cannot grok.  XXX This is all very kind-of hacky.
	// A proper solution would convert walk_tree to return ValuePtr's
	// instead of Handles. However, it seems this would require lots
	// of upcasting, which is horribly slow. So it seems better to
	// hold off on a "good fix", until the instantiate-to-values
	// experiment progresses further.  More generally, there are
	// several blockers:
	// * The need to instantiate in an atomspace (viz GetLink)
	//   impedes lazy evaluations.
	Type t = expr->get_type();
	if (nameserver().isA(t, VALUE_OF_LINK) or
	    nameserver().isA(t, ARITHMETIC_LINK))
	{
		// Perform substitution on non-numeric arguments before
		// applying the function itself.  We should not do any
		// eager evaluation here, for the numeric functions, as
		// these might be working with values, not atoms.
		//
		HandleSeq oset_results;
		for (const Handle& h: expr->getOutgoingSet())
		{
			Type th = h->get_type();
			if (nameserver().isA(th, VALUE_OF_LINK) or
			    nameserver().isA(th, ARITHMETIC_LINK))
			{
			   oset_results.push_back(h);
			}
			else
			{
				Handle hg(walk_tree(h, silent));
				if (hg) oset_results.push_back(hg);
			}
		}
		Handle flp(createLink(oset_results, t));
		ValuePtr pap(flp->execute(_as, silent));
		if (pap->is_atom())
			return _as->add_atom(HandleCast(pap));
		return pap;
	}

	// If there is a SatisfyingLink, we have to perform it
	// and return the satisfying set.
	if (nameserver().isA(t, SATISFYING_LINK))
	{
		return expr->execute(_as, silent);
	}

	// ExecutionOutputLinks
	if (nameserver().isA(t, EXECUTION_OUTPUT_LINK))
	{
		Handle eolh = reduce_exout(expr, silent);
		t = eolh->get_type();
		if (nameserver().isA(t, EXECUTION_OUTPUT_LINK))
		{
			ExecutionOutputLinkPtr geolp(ExecutionOutputLinkCast(eolh));
			return geolp->execute(_as, silent);
		}
		return eolh;
	}

	// The thread-links are ambiguously executable/evaluatable.
	if (nameserver().isA(t, PARALLEL_LINK))
	{
		return ValueCast(EvaluationLink::do_evaluate(_as, expr, silent));
	}

	// Instantiate.
	Handle grounded(walk_tree(expr, silent));

	// The returned handle is not yet in the atomspace. Add it now.
	// We do this here, instead of in walk_tree(), because adding
	// atoms to the atomspace is an expensive process.  We can save
	// some time by doing it just once, right here, in one big batch.
	// XXX FIXME Can we defer the addition to the atomspace to an even
	// later time??
	return _as->add_atom(grounded);
}

ValuePtr Instantiator::execute(const Handle& expr, bool silent)
{
	// Since we do not actually instantiate anything, we should not
	// consume quotations. (as it might change the semantics. (Huh ??))
	_consume_quotations = false;

	// XXX FIXME, since the variable map is empty, maybe we can do
	// something more efficient, here?
	ValuePtr vp(instantiate(expr, HandleMap(), silent));

#if NICE_IDEA_BUT_FAILS
	// If the result of execution is an evaluatable link, viz, something
	// that could return a truth value when evaluated, then do the
	// evaluation now, on the spot, and return the truth value.
	// There are several problems with this; the biggest is that
	// about 1/4th of the unit tests fail (39 out of 138). So just
	// collapsing the evaluatable/executable hierarchies into one
	// is not possible, without clarifying a lot of the back-n-forth
	// implicit casting, movement of data...
	//
	// That is, the current design of Atomese makes a large number
	// of implicit decisions about when things should and should not
	// be evaluated. Many of these decisions are buried in the link-types
	// themselves. Most of these implicit behaviors seem "natural", but
	// they also do not adhere to any grand plan ... its ad-hoc.
	// Thus, we cannot just mash together evaluation and execution
	// without reviewing all of these implicit and "natural" behaviors
	// and maybe modifying them.  For example, maybe we need some
	// new link types, like "EvaluateThisLink" and "ExecuteThisLink"
	// to force evaluation/executation at crtain points, instead of
	// just making it all implicit. Or maybe there is some other,
	// better design...

	// Evaluate, if possible.
	if (vp and nameserver().isA(vp->get_type(), EVALUATABLE_LINK))
	if (vp and nameserver().isA(vp->get_type(), CRISP_OUTPUT_LINK))

	// Evaluate, crisp-binary-boolean tv links, if possible.
	// This actually passes unit tests, but seems pointless, without
	// some corresponding grand design that unifies things correctly.
	if (vp)
	{
		Type t = vp->get_type();
		if (EQUAL_LINK == t or
		    IDENTICAL_LINK == t or
		    GREATER_THAN_LINK == t)
		{
			Handle h(HandleCast(vp));
			TruthValuePtr tvp(EvaluationLink::do_evaluate(_as, h));
			ValuePtr pap(ValueCast(tvp));
			return pap;
		}
	}
#endif

	return vp;
}

/* ===================== END OF FILE ===================== */
