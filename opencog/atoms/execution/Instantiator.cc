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

Instantiator::Instantiator(AtomSpace* as) : _as(as)
{}

/// Perform beta-reduction on the expression `expr`, using the `vmap`
/// to fish out values for variables.  The map holds pairs: the first
/// member of the pair is the variable; the second is the value that
/// should be used as its replacement.  (Note that "variables" do not
/// have to actually be VariableNode's; they can be any atom.)
static Handle beta_reduce(const Handle& expr, const GroundingMap& vmap)
{
	if (vmap.empty()) return expr;

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

/// Same as walk tree, except that it operates on a handle sequence,
/// instead of a single handle. The returned result is in oset_results.
/// Returns `true` if the results differ from the input, i.e. if the
/// result of execution/evaluation changed something.
bool Instantiator::walk_sequence(HandleSeq& oset_results,
                                 const HandleSeq& expr,
                                 Instate& ist) const
{
	bool changed = false;
	Context cp_context = ist._context;
	for (const Handle& h : expr)
	{
		Handle hg(walk_tree(h, ist));
		ist._context = cp_context;
		if (hg != h) changed = true;

		// GlobNodes are grounded by a ListLink of everything that
		// the GlobNode matches. Unwrap the list, and insert each
		// of the glob elements in sequence.
		Type ht = h->get_type();
		if (changed and
		    ((ist._context.is_unquoted() and GLOB_NODE == ht) or
		    ((UNQUOTE_LINK == ht and
		      GLOB_NODE == h->getOutgoingAtom(0)->get_type()))))
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

/// ExecutionOutputLinks get special treatment.
///
/// Even for the case of lazy execution, we still have to do eager
/// execution of the arguments passed to the ExOutLink.  This is
/// because the ExOutLink is a black box, and we cannot guess what
/// it might do.  It would be great if the authors of ExOutLinks
/// did the lazy execution themselves... but this is too much to
/// ask for. So we always eager-evaluate those args.
Handle Instantiator::reduce_exout(const Handle& expr,
                                  Instate& ist) const
{
	ExecutionOutputLinkPtr eolp(ExecutionOutputLinkCast(expr));

	// At this time, the GSN or the DSN is always in position 0
	// of the outgoing set, and the ListLink of arguments is always
	// in position 1.  Someday in the future, there may be a variable
	// declaration; we punt on that.
	Handle sn(eolp->get_schema());
	Handle args(eolp->get_args());

	sn = beta_reduce(sn, ist._varmap);

	// If its a DSN, obtain the correct body for it.
	if (DEFINED_SCHEMA_NODE == sn->get_type())
		sn = DefineLink::get_definition(sn);

	// If its an anonymous function link, execute it here.
	if (LAMBDA_LINK == sn->get_type())
	{
		LambdaLinkPtr flp(LambdaLinkCast(sn));

		// Three-step process. First, beta-reduce the args; second,
		// plug the args into the function. Third, execute (not here,
		// but by the caller).
		Handle body(flp->get_body());
		Variables vars(flp->get_variables());

		// Perform substitution on the args, only.
		args = beta_reduce(args, ist._varmap);

		// unpack list link
		const HandleSeq& oset(LIST_LINK == args->get_type() ?
				args->getOutgoingSet(): HandleSeq{args});

		return vars.substitute_nocheck(body, oset);
	}

#define PLN_NEEDS_UNQUOTING 1
#if PLN_NEEDS_UNQUOTING
	// PLN quotes its arguments, which now need to be unquoted.
	// This is required by PLNRulesUTest and specifically by
	// PLNRulesUTest::test_closed_lambda_introduction
	// PLNRulesUTest::test_implication_scope_to_implication
	// PLNRulesUTest::test_implication_and_lambda_factorization
	Type at0 = args->get_type();
	bool done = false;
	if ((LIST_LINK == at0 or IMPLICATION_LINK == at0) and
	     0 < args->get_arity())
	{
		Handle a1 = args->getOutgoingAtom(0);
		Type at1 = a1->get_type();
		if (QUOTE_LINK == at1 or
		    (IMPLICATION_LINK == at1 and
		     QUOTE_LINK == a1->getOutgoingAtom(0)->get_type()) or
		    (IMPLICATION_LINK == at0 and
		     QUOTE_LINK == args->getOutgoingAtom(1)->get_type()))
		{
			args = walk_tree(args, ist);
			done = true;
		}
	}

	// Perform substitution on the args, only.
	if (not done) args = beta_reduce(args, ist._varmap);
#else
	// Perform substitution on the args, only.
	args = beta_reduce(args, ist._varmap);
#endif

	Type t = expr->get_type();
	return createLink(t, sn, args);
}

/// walk_tree() performs a kind-of eager-evaluation of function arguments.
/// The code in here is a mashup of several different ideas that are not
/// cleanly separated from each other. (XXX FIXME, these need to be
/// cleanly seprated; its impeding overall clean design/implementation.)
/// Roughly, it goes like so:
///
/// First, walk downwards to the leaves of the tree. As we return back up,
/// if any free variables are encountered, then replace those variables
/// with the groundings held in `varmap`.
///
/// Second, during the above process, if any executable functions are
/// encountered, then execute them. This is "eager-execution".  The
/// results of that execution are plugged into the tree, and so we keep
/// returning upwards, back to the root.
///
/// The problem with eager execution is that it disallows recursive
/// functions: if `f(x)` itself calls `f`, then eager execution results
/// in the infinite loop `f(f(f(f(....))))` that never terminates, the
/// problem being that any possible termination condition inside of `f`
/// is never hit. (c.f. The textbook-classic recursive implementation of
/// factorial.)
///
/// This can be contrasted with `beta_reduce()` up above, which performs
/// the substitution only, but does NOT perform an execution at all.
///
/// So, here's the funny bit: sometimes, `walk_tree` does do
/// lazy-execution, sometimes. In the current version, when it
/// encounters a function to be executed, it mostly just performs the
/// substitution on the function args, and then executes the function.
/// Its up to the function itself to get more done, as needed.
///
Handle Instantiator::walk_tree(const Handle& expr,
                               Instate& ist) const
{
	Type t = expr->get_type();

	// Store the current context so we can update it for subsequent
	// recursive calls of walk_tree.
	Context context_cp(ist._context);
	ist._context.update(expr);

	// Discard the following QuoteLink, UnquoteLink or LocalQuoteLink
	// as it is serving its quoting or unquoting function.
	if ((ist._consume_quotations or ist._needless_quotation) and
	    context_cp.consumable(t))
	{
		if (1 != expr->get_arity())
			throw InvalidParamException(TRACE_INFO,
			                            "QuoteLink/UnquoteLink has "
			                            "unexpected arity!");
		Handle child = expr->getOutgoingAtom(0);
		Handle walked_child = walk_tree(child, ist);

		// Only consume if the quotation is really needless (walking
		// the children might have changed _needless_quotation).
		if (ist._consume_quotations or ist._needless_quotation)
			return walked_child;

		// Otherwise keep the quotation, but set _needless_quotation
		// back to true for the remaining tree
		ist._needless_quotation = true;
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
				ist._needless_quotation = false;

			return expr;
		}

		// If we are here, we are a Node.
		if (DEFINED_SCHEMA_NODE == t)
		{
			return walk_tree(DefineLink::get_definition(expr), ist);
		}

		if (VARIABLE_NODE != t and GLOB_NODE != t)
			return expr;

		// If it is a quoted or shadowed variable don't substitute.
		// TODO: what about globs?
		if (VARIABLE_NODE == t and not context_cp.is_free_variable(expr))
			return expr;

		// If we are here, we found a free variable or glob. Look
		// it up. Return a grounding if it has one, otherwise return
		// the variable itself.
		GroundingMap::const_iterator it = ist._varmap.find(expr);
		if (ist._varmap.end() == it) return expr;

		// Not so fast, pardner. VariableNodes can be grounded by
		// links, and those links may be executable. In that case,
		// we have to execute them.

		// halt infinite regress
		if (ist._halt)
			return expr;

		ist._halt = true;
		Handle hgnd(walk_tree(it->second, ist));
		ist._halt = false;
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
			ist._needless_quotation = false;
		goto mere_recursive_call;
	}

	// Reduce PutLinks.
	if (PUT_LINK == t)
	{
		// Step one: perform variable substituions
		Handle hexpr(beta_reduce(expr, ist._varmap));
		PutLinkPtr ppp(PutLinkCast(hexpr));

		// Step two: beta-reduce.
		Handle red(HandleCast(ppp->execute(_as, ist._silent)));

		// TODO -- Maybe the PutLink should also do everything below,
		// itself? i.e. we should not have to do the below for it,
		// right? The right answer is somewhat ... hazy.
		if (nullptr == red)
			return red;

		// Step three: execute the resulting body.
		// (unless its not executable)
		if (DONT_EXEC_LINK == red->get_type())
			return red->getOutgoingAtom(0);

		Handle rex(walk_tree(red, ist));
		if (nullptr == rex)
			return rex;

		// Step four: XXX this is awkward, but seems to be needed...
		// If the result is evaluatable, then evaluate it. e.g. if the
		// result has a GroundedPredicateNode, we need to run it now.
		// Anyway, do_evaluate() will throw if rex is not evaluatable.
		//
		// The DontExecLink is a weird hack to halt evaluation.
		// We unwrap it and throw it away when encountered.
		// Some long-term fix is needed that avoids this step-four
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
					unwrap.push_back(plo);
				}
			}
			return createLink(std::move(unwrap), SET_LINK);
		}
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
			vardecl = walk_tree(vardecl, ist);

		// Recursively walk body, making sure quotation is preserved
		Handle body = ll->get_body();
		// If the lambda is ill-formed it might not have a body, throw
		// an exception then
		if (not body)
		{
			if (ist._silent)
				throw NotEvaluatableException();
			throw SyntaxException(TRACE_INFO, "body is ill-formed");
		}

		Type bt = body->get_type();
		if (Quotation::is_quotation_type(bt))
		{
			ist._context.update(body);
			ist._needless_quotation = false;
			body = walk_tree(body->getOutgoingAtom(0), ist);
			body = createLink(bt, body);
			ist._needless_quotation = true;
		}
		else
		{
			body = walk_tree(body, ist);
		}

		// Reconstruct Lambda, if it has changed
		if (ll->get_vardecl() != vardecl or ll->get_body() != body)
		{
			HandleSeq oset{body};
			if (vardecl)
				oset.insert(oset.begin(), vardecl);
			// TODO: copy values
			return createLink(std::move(oset), LAMBDA_LINK);
		}
		return expr;
	}

	// Handle DeleteLink's before general FunctionLink's; they
	// work differently.
	if (DELETE_LINK == t)
	{
		HandleSeq oset_results;
		walk_sequence(oset_results, expr->getOutgoingSet(), ist);
		for (const Handle& h: oset_results)
		{
			Type ht = h->get_type();
			if (VARIABLE_NODE != ht and GLOB_NODE != ht)
				_as->extract_atom(h, true);
		}
		return Handle::UNDEFINED;
	}

	// Ideally, we should not evaluate any EvaluatableLinks.
	// However, some of these may hold embedded executable links
	// inside of them, which the current unit tests and code
	// expect to be executed.  Thus, for right now, we only avoid
	// evaluating VirtualLinks, as these all are capable of their
	// own lazy-evaluation, and so, if evaluation is needed,
	// it will be triggered by something else. We do, of course,
	// substitute in for free variables, if any.
	//
	// Non-virtual evaluatables fall through and are handled
	// below.
	//
	// if (nameserver().isA(t, EVALUATABLE_LINK)) ... not now...
	if (nameserver().isA(t, VIRTUAL_LINK))
		return beta_reduce(expr, ist._varmap);

	// ExecutionOutputLinks
	if (nameserver().isA(t, EXECUTION_OUTPUT_LINK))
	{
		Handle eolh = reduce_exout(expr, ist);
		return HandleCast(eolh->execute(_as, ist._silent));
	}

	// Fire any other function links, not handled above.
	if (nameserver().isA(t, FUNCTION_LINK))
	{
		Handle flh = beta_reduce(expr, ist._varmap);

		// Some function links are guaranteed to return values.
		// We cannot/must not execute them here.
		Type tbr = flh->get_type();
		if (nameserver().isA(tbr, VALUE_OF_LINK) or
		    nameserver().isA(tbr, SET_VALUE_LINK)) return flh;

		return HandleCast(flh->execute(_as, ist._silent));
	}

	// If there is a SatisfyingLink (e.g. GetLink, BindLink, etc.),
	// we have to perform it and return the satisfying set.
	if (nameserver().isA(t, SATISFYING_LINK))
	{
		// XXX I don't get it... don't we need to perform var
		// substitution here? Is this just not tested?
		// beta_reduce(expr, ist._varmap);
		return HandleCast(expr->execute(_as, ist._silent));
	}

	// If there is a JoinLink
	// we have to perform it and return the satisfying set.
	if (nameserver().isA(t, JOIN_LINK))
	{
		// XXX I don't get it... don't we need to perform var
		// substitution here? Is this just not tested?
		// beta_reduce(expr, ist._varmap);
		return HandleCast(expr->execute(_as, ist._silent));
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
		return beta_reduce(expr->getOutgoingAtom(0), ist._varmap);
#else
		return beta_reduce(expr, ist._varmap);
#endif
	}

	// None of the above. Create a duplicate link, but with an outgoing
	// set where the variables have been substituted by their values.
mere_recursive_call:
	HandleSeq oset_results;
	bool changed = walk_sequence(oset_results, expr->getOutgoingSet(), ist);
	if (changed)
	{
		Handle subl(createLink(std::move(oset_results), t));
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
                                   const GroundingMap& varmap,
                                   bool silent) const
{
	// throw, not assert, because this is a user error ...
	if (nullptr == expr)
		throw InvalidParamException(TRACE_INFO,
			"Asked to ground a null expression");

	Instate ist(varmap);
	ist._silent = silent;

	// Since we do not actually instantiate anything, we should not
	// consume quotations (as it might change the semantics.)
	// We are not instantiating anything, because the map is empty.
	if (0 == varmap.size())
		ist._consume_quotations = false;

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
	    nameserver().isA(t, SET_VALUE_LINK) or
	    nameserver().isA(t, ARITHMETIC_LINK))
	{
		HandleSeq oset_results;
		for (const Handle& h: expr->getOutgoingSet())
		{
			Handle hg(walk_tree(h, ist));

			// Globs will return a matching list. Arithmetic
			// links will choke on lists, so expand them.
			if (GLOB_NODE == h->get_type())
			{
				for (const Handle& gg : hg->getOutgoingSet())
					oset_results.push_back(gg);
			}
			else
				oset_results.push_back(hg);
		}
		Handle flp(createLink(std::move(oset_results), t));
		ValuePtr pap(flp->execute(_as, silent));
		if (_as and pap->is_atom())
			return _as->add_atom(HandleCast(pap));
		return pap;
	}

	// If there is a SatisfyingLink, we have to perform it
	// and return the satisfying set.
	if (nameserver().isA(t, SATISFYING_LINK) or
	    nameserver().isA(t, JOIN_LINK))
	{
		if (0 == varmap.size())
			return expr->execute(_as, silent);

		// There are vars to be beta-reduced. Reduce them.
		Handle grounded(walk_tree(expr, ist));
		if (_as) grounded = _as->add_atom(grounded);
		return grounded->execute(_as, silent);
	}

	// ExecutionOutputLinks
	if (nameserver().isA(t, EXECUTION_OUTPUT_LINK))
	{
		// XXX Don't we need to plug in the vars, first!?
		// Maybe this is just not tested?
		Handle eolh = reduce_exout(expr, ist);
		if (not eolh->is_executable()) return eolh;
		eolh = _as->add_atom(eolh);
		return eolh->execute(_as, silent);
	}

	// ExecuteThreadedLinks
	if (EXECUTE_THREADED_LINK == t)
	{
		// XXX Don't we need to plug in the vars, first!?
		// Maybe this is just not tested?
		return expr->execute(_as, silent);
	}

	// The thread-links are ambiguously executable/evaluatable.
	if (nameserver().isA(t, PARALLEL_LINK))
	{
		// XXX Don't we need to plug in the vars, first!?
		// Yes, we do, but this is just not tested, right now.
		return ValueCast(EvaluationLink::do_evaluate(_as, expr, silent));
	}

	// Execute any DefinedPredicateNodes
	if (nameserver().isA(t, DEFINED_PREDICATE_NODE))
	{
		// XXX Don't we need to plug in the vars, first!?
		// Maybe this is just not tested?
		return ValueCast(EvaluationLink::do_evaluate(_as, expr, silent));
	}

	// Instantiate.
	Handle grounded(walk_tree(expr, ist));

	// The returned handle is not yet in the atomspace. Add it now.
	// We do this here, instead of in walk_tree(), because adding
	// atoms to the atomspace is an expensive process.  We can save
	// some time by doing it just once, right here, in one big batch.
	// XXX FIXME Can we defer the addition to the atomspace to an even
	// later time??
	if (_as) return _as->add_atom(grounded);
	return grounded;
}

ValuePtr Instantiator::execute(const Handle& expr, bool silent)
{
	// Make sure that the atom is in an atomspace that is compatible
	// with the execution environment. When it's not, then bizzare
	// results happen (e.g. with searches, because the search cannot
	// find atoms in the correct atomspace.) We do allow, for now,
	// atoms with null atomspaces; if/when these need to be inserted
	// somewhere, that will be done when needed, into the right place.
	AtomSpace* exas = expr->getAtomSpace();
	if (nullptr != exas and not _as->in_environ(expr))
		throw RuntimeException(TRACE_INFO,
			"Can't execute: current AtomSpace is %lu but atom is in AtomSpace %lu",
			_as->get_uuid(), exas->get_uuid());

	// Try to execute directly, if possible. Not everything is
	// capable of this, yet, but the FunctionLinks all do seem to work.
	//
	// if (expr->is_executable())
	if (nameserver().isA(expr->get_type(), FUNCTION_LINK) or
	    nameserver().isA(expr->get_type(), SATISFYING_LINK) or
	    nameserver().isA(expr->get_type(), JOIN_LINK))
	{
		ValuePtr vp = expr->execute(_as, silent);
		if (vp->is_atom())
			return _as->add_atom(HandleCast(vp));
		return vp;
	}

	// XXX FIXME, we need to get rid of this call entirely, and just
	// return expr->execute(_as, silent) instead, like above.
	// However, assorted parts are still broken and don't work.
	ValuePtr vp(instantiate(expr, GroundingMap(), silent));

	// PutLink is incompletely evaluated, above. Finish the job here.
	if (vp and vp->is_atom())
	{
		Handle h(HandleCast(vp));
		if (h->is_executable()) return h->execute();
	}

	return vp;
}

/* ===================== END OF FILE ===================== */
