/*
 * opencog/atoms/execution/EvaluationLink.cc
 *
 * Copyright (C) 2009, 2013, 2014, 2015 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
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
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/core/PutLink.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/execution/GroundedProcedureNode.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/flow/TruthValueOfLink.h>
#include <opencog/atoms/flow/PredicateFormulaLink.h>
#include <opencog/atoms/parallel/ParallelLink.h>
#include <opencog/atoms/parallel/ThreadJoinLink.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/reduct/FoldLink.h>
#include <opencog/atoms/reduct/NumericFunctionLink.h>
#include <opencog/atoms/truthvalue/FormulaTruthValue.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/atoms/value/LinkValue.h>

#include <opencog/atomspace/AtomSpace.h>

#include "Force.h"
#include "EvaluationLink.h"

using namespace opencog;

EvaluationLink::EvaluationLink(const HandleSeq&& oset, Type t)
    : FreeLink(std::move(oset), t)
{
	if (not nameserver().isA(t, EVALUATION_LINK))
		throw RuntimeException(TRACE_INFO,
		    "Expecting an EvaluationLink or an inherited type thereof");

	// The "canonical" EvaluationLink structure is:
	//    EvaluationLink
	//        PredicateNode "foo"
	//        ListLink
	//           ...
	//
	// However, patterns can have variables for either the
	// ListLink, or the PredicateNode, or both.
	//
	// ... except reality is worse: many examples include
	// badly-formed EvaluationLinks, on-purpose.  So, before
	// we can do any sort of strict checking here, we would
	// need fix all those wiki pages, examples, etc.
	// As of this writing (March 2017), there are seven unit
	// tests that create EvaluationLinks whose size() is not 2:
	//    PutLinkUTest GetLinkUTest BuggySelfGroundUTest
	//    StackMoreUTest ConstantClausesUTest PersistUTest
	//    MultiPersistUTest
	//
/********
	if (2 != oset.size())
	   // or (LIST_LINK != oset[1]->get_type()))
	{
		throw RuntimeException(TRACE_INFO,
		    "EvaluationLink must have predicate and args!");
	}
*********/
}

EvaluationLink::EvaluationLink(const Handle& schema, const Handle& args)
    : FreeLink({schema, args}, EVALUATION_LINK)
{
	if (LIST_LINK != args->get_type()) {
		throw RuntimeException(TRACE_INFO,
		    "EvaluationLink must have args in a ListLink!");
	}
}

/// We get exceptions in two differet ways: (a) due to user error,
/// in which case we need to report the error to the user, and
/// (b) occasionally expected errors, which might occur during normal
/// processing, and should be ignored. The "normal" errors should not
/// be reported to the user; nor should they be printed to the log-file.
/// Using a try-catch block is enough to prevent them from being passed
/// to the user; but it is not enough to prevent them from printing.
/// Thus, we use a bool flag to not print. (It would be nice if C++
/// offered a way to automate this in the catch-block, so that the
/// pesky "silent" flag was not needed.)
///
/// DefaultPatternMatchCB.cc and also Instantiator.cc both catch
/// the NotEvaluatableException thrown here.  Basically, these
/// know that they might be sending non-evaluatable atoms here, and
/// don't want to garbage up the log files with bogus errors.
///
static void throwSyntaxException(bool silent, const char* message...)
{
	if (silent)
		throw NotEvaluatableException();
	va_list args;
	va_start(args, message);
	throw SyntaxException(TRACE_INFO, message, args);
	va_end(args);
}

/// Extract a single floating-point double out of an atom, that,
/// when executed, should yield a value containing a number.
/// Viz, either a NumberNode, or a FloatValue.
static double get_numeric_value(AtomSpace* as, bool silent,
                                Handle h)
{
	ValuePtr pap(NumericFunctionLink::get_value(as, silent, h));
	Type t = pap->get_type();

	if (NUMBER_NODE == t)
	{
		NumberNodePtr n(NumberNodeCast(pap));
		return n->get_value();
	}

	if (nameserver().isA(t, FLOAT_VALUE))
	{
		FloatValuePtr fv(FloatValueCast(pap));
		if (fv->value().empty())
			throw RuntimeException(TRACE_INFO, "FloatValue is empty!");
		return fv->value()[0];
	}

	throwSyntaxException(silent,
		"Don't know how to do arithmetic with this: %s",
		pap->to_string().c_str());

	return std::nan("");
}

/// Perform a GreaterThan check
static bool greater(AtomSpace* as, const Handle& h, bool silent)
{
	const HandleSeq& oset = h->getOutgoingSet();
	if (2 != oset.size())
		throw SyntaxException(TRACE_INFO,
		     "GreaterThankLink expects two arguments");

	double v0 = get_numeric_value(as, silent, oset[0]);
	double v1 = get_numeric_value(as, silent, oset[1]);

	return (v0 > v1);
}

static bool lesser(AtomSpace* as, const Handle& h, bool silent)
{
	const HandleSeq& oset = h->getOutgoingSet();
	if (2 != oset.size())
		throw SyntaxException(TRACE_INFO,
		     "GreaterThankLink expects two arguments");

	double v0 = get_numeric_value(as, silent, oset[0]);
	double v1 = get_numeric_value(as, silent, oset[1]);

	return (v0 < v1);
}

/// Perform a IsClosed check
static bool is_outgoing_closed(const Handle& h)
{
	const HandleSeq& oset = h->getOutgoingSet();
	return std::all_of(oset.begin(), oset.end(),
	                   [](const Handle& o) { return is_closed(o); });
}

/// Perform the IsTrueLink check
static bool is_outgoing_true(AtomSpace* scratch, const Handle& h)
{
	// Truth values are always relative to the AtomSpace the Atom is in.
	// So make sure that the Atom is in the AtomSpace.
	Handle hs(scratch->add_atom(h));
	const HandleSeq& oset = hs->getOutgoingSet();
	return std::all_of(oset.begin(), oset.end(),
		[](const Handle& o)
			{ return *o->getTruthValue() == *TruthValue::TRUE_TV(); });
}

/// Perform the IsFalseLink check
static bool is_outgoing_false(AtomSpace* scratch, const Handle& h)
{
	// Truth values are always relative to the AtomSpace the Atom is in.
	// So make sure that the Atom is in the AtomSpace.
	Handle hs(scratch->add_atom(h));
	const HandleSeq& oset = hs->getOutgoingSet();
	return std::all_of(oset.begin(), oset.end(),
		[](const Handle& o)
			{ return *o->getTruthValue() == *TruthValue::FALSE_TV(); });
}

static ValuePtr exec_or_eval(AtomSpace* as,
                             const Handle& term,
                             AtomSpace* scratch,
                             bool silent)
{
	if (nameserver().isA(term->get_type(), EVALUATABLE_LINK))
	{
		try
		{
			return ValueCast(EvaluationLink::do_eval_scratch(as,
			                    term, scratch, silent));
		}
		catch (const SilentException& ex)
		{
			return term;
		}
	}

	Instantiator inst(as);
	ValuePtr vp(inst.execute(term, silent));

	// If the return value is a QueueValue, we assume that this
	// is the result of executing a MeetLink or QueryLink.
	// In this case, unwrap it, to get the "actual value".
	// This feels slightly hacky, but will do for just right now.
	if (QUEUE_VALUE == vp->get_type())
	{
		HandleSeq hs(LinkValueCast(vp)->to_handle_seq());
		if (1 == hs.size())
			vp = hs[0];
	}
	if (vp->is_atom()) scratch->add_atom(HandleCast(vp));
	return vp;
}

/// Check for syntactic equality. Specifically, when comparing
/// atoms, the handles MUST be the same handle.
/// If there are two or more elements, they must ALL be equal.
static bool identical(const Handle& h)
{
	const HandleSeq& oset = h->getOutgoingSet();
	size_t nelts = oset.size();
	if (2 > nelts) return true;

	for (size_t j=1; j<nelts; j++)
	{
		if (oset[0] != oset[j]) return false;
	}
	return true;
}

/// Check for semantic equality. Specifically, when comparing
/// atoms, then handles might be different, but the contents must
/// compare as being the same, after the evaluation of the contents.
/// If there are two or more elements, they must ALL be equal.
static bool equal(AtomSpace* as, const Handle& h, bool silent)
{
	const HandleSeq& oset = h->getOutgoingSet();
	size_t nelts = oset.size();
	if (2 > nelts) return true;

	ValuePtr v0(exec_or_eval(as, oset[0], as, silent));

	for (size_t j=1; j<nelts; j++)
	{
		ValuePtr v1(exec_or_eval(as, oset[j], as, silent));
		if (v0 != v1 and *v0 != *v1) return false;
	}
	return true;
}

/// Check for alpha equivalence. If the link contains no free
/// variables, then this behaves the same as EqualLink. If the
/// link does contain free variables, and they are in the same
/// location, and can be alpha-converted to one-another, then yes,
/// they're equal. If the two expressions cannot be alpha-converted
/// one into another, then false.
static bool alpha_equal(AtomSpace* as, const Handle& h, bool silent)
{
	const HandleSeq& oset = h->getOutgoingSet();
	if (2 != oset.size())
		throw SyntaxException(TRACE_INFO,
		     "AlphaEqualLink expects two arguments");

	Instantiator inst(as);
	Handle h0(HandleCast(inst.execute(oset[0], silent)));
	Handle h1(HandleCast(inst.execute(oset[1], silent)));

	// Are they strictly equal? Good!
	if (h0 == h1)
		return true;

	// Not strictly equal. Are they alpha convertible?
	Variables v0, v1;
	v0.find_variables(h0);
	v1.find_variables(h1);

	// If the variables are not alpha-convertable, then
	// there is no possibility of equality.
	if (not v0.is_equal(v1))
		return false;

	// Actually alpha-convert, and compare.
	Handle h1a = v1.substitute_nocheck(h1, v0.varseq, silent);
	return (*((AtomPtr)h0) == *((AtomPtr)h1a));
}

/// Check for set membership
static bool member(AtomSpace* as, const Handle& h, bool silent)
{
	const HandleSeq& oset = h->getOutgoingSet();
	if (2 != oset.size())
		throw SyntaxException(TRACE_INFO,
		     "MemberLink expects two arguments");

	ValuePtr v0(exec_or_eval(as, oset[0], as, silent));
	ValuePtr v1(exec_or_eval(as, oset[1], as, silent));

	// Is v0 a member of v1?  This question makes sense only
	// if v0 is an atom and v1 is a set.
	if (not v0->is_atom()) return false;
	if (not nameserver().isA(v1->get_type(), SET_LINK)) return false;

	for (const Handle& hs: HandleCast(v1)->getOutgoingSet())
	{
		if (v0 == hs) return true;
	}

	return false;
}

/// Check for subset relationship
static bool subset(AtomSpace* as, const Handle& h, bool silent)
{
	const HandleSeq& oset = h->getOutgoingSet();
	if (2 != oset.size())
		throw SyntaxException(TRACE_INFO,
		     "SubsetLink expects two arguments");

	ValuePtr v0(exec_or_eval(as, oset[0], as, silent));
	ValuePtr v1(exec_or_eval(as, oset[1], as, silent));

	// Is v0 a subset of v1?  This question makes sense only
	// if v0 and v1 are sets.
	if (not nameserver().isA(v0->get_type(), SET_LINK)) return false;
	if (not nameserver().isA(v1->get_type(), SET_LINK)) return false;

	const HandleSeq& superset(HandleCast(v1)->getOutgoingSet());
	for (const Handle& h0: HandleCast(v0)->getOutgoingSet())
	{
		bool found = false;
		for (const Handle& h1 : superset)
		{
			if (h0 == h1) {found = true; break;}
		}
		if (not found) return false;
	}

	return true;
}

/// Check to make sure all atoms differ
static bool exclusive(AtomSpace* as, const Handle& h, bool silent)
{
	HandleSeq oset(h->getOutgoingSet());
	ValueSeq vset;

	size_t olen = oset.size();
	while (true)
	{
		if (2 > olen) return true;

		Handle last(oset.back());
		oset.pop_back();
		olen --;

		ValuePtr v0(exec_or_eval(as, last, as, silent));
		for (size_t j=0; j< olen; j++)
		{
			if (vset.size() <= j)
				vset.push_back(exec_or_eval(as, oset[j], as, silent));
			if (v0 == vset[j] or *v0 == *vset[j]) return false;
		}
	}
}

/** Return true if the SatisfactionLink can be "trivially" evaluated. */
static bool is_evaluatable_sat(const Handle& satl)
{
	if (1 != satl->get_arity())
		return false;

	PatternLinkPtr plp(PatternLinkCast(satl));

	return 0 == plp->get_variables().varseq.size();
}

/** Return true, if `thish` is tail-recursive */
static bool is_tail_rec(const Handle& thish, const Handle& tail)
{
	if (DEFINED_PREDICATE_NODE != tail->get_type())
		return false;

	Handle defn(DefineLink::get_definition(tail));
	if (defn == thish)
		return true;

	if (SATISFACTION_LINK != defn->get_type())
		return false;

	if (not is_evaluatable_sat(defn))
		return false;

	if (thish == defn->getOutgoingAtom(0))
		return true;

	return false;
}

static TruthValuePtr bool_to_tv(bool truf)
{
	if (truf) return TruthValue::TRUE_TV();
	return TruthValue::FALSE_TV();
}

/// `crisp_eval_scratch()` -- evaluate any Atoms that can meaningfully
/// result in a crisp-logic, binary true/false truth value.
///
/// There are two general kinds "truth values" that we are concerned
/// about.  For many cases, the "truth value" is explicitly a crisp,
/// binary Boolean-logic truth value, being either "true" or "false"
/// and having no other qusi-ambiguous, fuzzy or probabilistic
/// interpretation. Examples include the logical constants TrueLink,
/// FalseLink, and the logical connectives NotLink, AndLink, OrLink.
/// Yes, it is possible for these to have other interpretations, e.g.
/// probabilistic interpretations. That is not what we are doing here:
/// we are working with uninterpreted logical constants and connectives.
/// The `crisp_eval_scratch()` function handles the evaluation of Atoms
/// that have a natural crisp-truth interpretation.
///
/// A different class of Atoms will naturally have fuzzy or
/// probabilistic valuations associated with them. These are evaluated
/// by the `do_eval_scratch()` function.
///
/// Both kinds can be mixed together with one-another. An implicit
/// conversion from crisp-to-fuzzy and back is performed, when needed,
/// when appropriate. Maybe this is a design flaw? Maybe we should force
/// the user to declare an explicit conversion?
///
/// The implementation here is one big giant case-statement. It works.
/// In the long-run, it might be better to just have C++ classes for
/// each distinct atom type, and have an `evaluate()` method on each.
/// Whatever. Performance is probably about the same, and for just right
/// now, this is straight-forward and it works.
///
/// This function takes TWO atomspace arguments!  The first is the
/// "main" atomspace, the second is a "scratch" or "temporary"
/// atomspace.  The scratch space is used to instantiate any arguments
/// that need to be passed to evaluatable links (i.e. to predicates);
/// the idea is that such temporaries don't add garbage to the main
/// atomspace.  The first argument, though, the "main" space, is used
/// to instantiate any executable atoms: specifically, any PutLinks
/// that were wrapped up by TrueLink, FalseLink. This is needed to get
/// SequentialAndLink to work correctly, when moving down the sequence.
///
static bool crispy_eval_scratch(AtomSpace* as,
                                const Handle& evelnk,
                                AtomSpace* scratch,
                                bool silent);

static bool crispy_maybe(AtomSpace* as,
                         const Handle& evelnk,
                         AtomSpace* scratch,
                         bool silent,
                         bool& failed)
{
	failed = false;

	Type t = evelnk->get_type();

	// Logical constants
	if (TRUE_LINK == t or FALSE_LINK == t)
	{
		// Assume that the link is wrapping something executable (or
		// evaluatable), which we execute (or evaluate), but then
		// ignore the result.  The executable ones, we need to put the
		// result in the (scratch) atomspace ... but in either case,
		// we ignore the TV on it. We are doing this for the side-effects,
		// of course -- the True/FalseLinks are pure side-effect atoms.
		//
		// We instantiate/evaluate in the main atomspace, however.
		// This is subtle, so listen-up: one of the side effects
		// might involve evaluating some condition, which then pokes
		// atoms into the atomspace, to signal some event or state.
		// These cannot be discarded. This is explicitly tested by
		// SequenceUTest::test_or_put().
		for (const Handle& term : evelnk->getOutgoingSet())
			exec_or_eval(as, term, scratch, silent);

		if (TRUE_LINK == t) return true;
		return false;
	}

	// -------------------------
	// Crisp-binary-valued Boolean Logical connectives
	if (NOT_LINK == t)
	{
		return not crispy_eval_scratch(as,
		      evelnk->getOutgoingAtom(0), scratch, silent);
	}
	else if (AND_LINK == t)
	{
		for (const Handle& h : evelnk->getOutgoingSet())
		{
			bool tv = crispy_eval_scratch(as, h, scratch, silent);
			if (not tv) return false;
		}
		return true;
	}
	else if (OR_LINK == t)
	{
		for (const Handle& h : evelnk->getOutgoingSet())
		{
			bool tv = crispy_eval_scratch(as, h, scratch, silent);
			if (tv) return true;
		}
		return false;
	}
	else if (SEQUENTIAL_AND_LINK == t)
	{
		const HandleSeq& oset = evelnk->getOutgoingSet();
		size_t arity = oset.size();
		if (0 == arity) return true;

		// Is this tail-recursive? If so, then handle it.
		bool is_trec = is_tail_rec(evelnk, oset[arity-1]);
		if (is_trec) arity--;

		// Loop at least once. If tail-recursive, loop forever.
		do
		{
			for (size_t i=0; i<arity; i++)
			{
				bool tv = crispy_eval_scratch(as, oset[i], scratch, silent);
				if (not tv) return false;
			}
		} while (is_trec);
		return true;
	}
	else if (SEQUENTIAL_OR_LINK == t)
	{
		const HandleSeq& oset = evelnk->getOutgoingSet();
		size_t arity = oset.size();
		if (0 == arity) return false;

		// Is this tail-recursive? If so, then handle it.
		bool is_trec = is_tail_rec(evelnk, oset[arity-1]);
		if (is_trec) arity--;

		// Loop at least once. If tail-recurive, loop forever.
		do
		{
			for (size_t i=0; i<arity; i++)
			{
				bool tv = crispy_eval_scratch(as, oset[i], scratch, silent);
				if (tv) return true;
			}
		} while (is_trec);
		return false;
	}

	// -------------------------
	// Assorted relations
	if (IDENTICAL_LINK == t) return identical(evelnk);
	if (EQUAL_LINK == t) return equal(scratch, evelnk, silent);
	if (ALPHA_EQUAL_LINK == t) return alpha_equal(scratch, evelnk, silent);
	if (GREATER_THAN_LINK == t) return greater(scratch, evelnk, silent);
	if (LESS_THAN_LINK == t) return lesser(scratch, evelnk, silent);
	if (IS_CLOSED_LINK == t) return is_outgoing_closed(evelnk);
	if (IS_TRUE_LINK == t) return is_outgoing_true(scratch, evelnk);
	if (IS_FALSE_LINK == t) return is_outgoing_false(scratch, evelnk);
	if (MEMBER_LINK == t) return member(scratch, evelnk, silent);
	if (SUBSET_LINK == t) return subset(scratch, evelnk, silent);
	if (EXCLUSIVE_LINK == t) return exclusive(scratch, evelnk, silent);

	// -------------------------
	// Multi-threading primitives
	if (THREAD_JOIN_LINK == t)
	{
		ThreadJoinLinkPtr tjlp = ThreadJoinLinkCast(evelnk);
		return tjlp->evaluate(as, silent, scratch);
	}
	else if (PARALLEL_LINK == t)
	{
		ParallelLinkPtr plp = ParallelLinkCast(evelnk);
		plp->evaluate(as, silent, scratch);
		return true;
	}

	if (nameserver().isA(t, CRISP_OUTPUT_LINK) and
	    evelnk->is_evaluatable())
	{
		TruthValuePtr tv(evelnk->evaluate(scratch, silent));
		if (0.5 < tv->get_mean()) return true;
		return false;
	}

	// A handful of link types that should be auto-converted into
	// crisp truth values.
	if (EVALUATION_LINK == t or
	    DEFINED_PREDICATE_NODE == t)
	{
		TruthValuePtr tv(EvaluationLink::do_eval_scratch(as,
		                 evelnk, scratch, silent));
		if (0.5 < tv->get_mean()) return true;
		return false;
	}

	failed = true;
	return false;
}

static bool crispy_eval_scratch(AtomSpace* as,
                                const Handle& evelnk,
                                AtomSpace* scratch,
                                bool silent)
{
	bool failed;
	bool tf = crispy_maybe(as, evelnk, scratch, silent, failed);
	if (not failed)
		return tf;

	throwSyntaxException(silent,
		"Either incorrect or not implemented yet. Cannot evaluate %s",
		evelnk->to_string().c_str());

	return false;
}


static TruthValuePtr reduce_formula(const Handle& pred,
                                    const HandleSeq& args)
{
	HandleSeq reduced;
	for (Handle flh : pred->getOutgoingSet())
	{
		if (LAMBDA_LINK == flh->get_type())
			flh = LambdaLinkCast(flh)->beta_reduce(args);

		if (nameserver().isA(flh->get_type(), FUNCTION_LINK))
		{
			// The FunctionLink presumably has free variables in it.
			// Reduce them with the provided arguments.
			FunctionLinkPtr flp(FunctionLinkCast(flh));
			const FreeVariables& fvars = flp->get_vars();
			if (not fvars.empty())
				flh = fvars.substitute_nocheck(flh, args);
		}
		else
			// We expected something executable...
			throw SyntaxException(TRACE_INFO,
				"Expecting a Lambda or FunctionLink");

		// Wherever the args live, the reduced formula must
		// live there also: it's lifettime must be identical
		// to the args.
		AtomSpace* as = args[0]->getAtomSpace();
		flh = as->add_atom(flh);
		reduced.push_back(flh);
	}

	return createFormulaTruthValue(std::move(reduced));
}

/// `do_eval_with_args()` -- evaluate a PredicateNode with arguments.
///
/// Expects "pn" to be any actively-evaluatable predicate type.
///     Currently, this includes the GroundedPredicateNode, the
///     DefinedPredicateNode and the PredicateFormulasLink.
/// Expects "args" to be a ListLink. These arguments will be
///     substituted into the predicate.
///
/// For the special case of GroundedPredicateNode, the arguments are
/// "eager-evaluated", because it is assumed that the GPN is unaware
/// of the concept of lazy evaluation, and can't do it itself.  In
/// all other cases, lazy evaluation is done (i.e. no evaluation is
/// done, if it is not needed.)
///
/// The arguments are then inserted into the predicate, and the
/// predicate as a whole is then evaluated.
///
TruthValuePtr do_eval_with_args(AtomSpace* as,
                                const Handle& pn,
                                const HandleSeq& cargs,
                                bool silent)
{
	Type pntype = pn->get_type();
	if (DEFINED_PREDICATE_NODE == pntype)
	{
		Handle defn = DefineLink::get_definition(pn);
		Type dtype = defn->get_type();

		// Allow recursive definitions. This can be handy.
		while (DEFINED_PREDICATE_NODE == dtype)
		{
			defn = DefineLink::get_definition(defn);
			dtype = defn->get_type();
		}

		if (PREDICATE_FORMULA_LINK == dtype)
			return PredicateFormulaLinkCast(defn)->apply(as, cargs, silent);

		if (DYNAMIC_FORMULA_LINK == dtype)
			return reduce_formula(defn, cargs);

		// If its not a LambdaLink, then I don't know what to do...
		if (LAMBDA_LINK != dtype)
			throw SyntaxException(TRACE_INFO,
				"Expecting definition to be a LambdaLink, got %s",
				defn->to_string().c_str());

		// Treat LambdaLink as if it were a PutLink -- perform
		// the beta-reduction, and evaluate the result.
		LambdaLinkPtr lam(LambdaLinkCast(defn));
		Handle reduct(lam->beta_reduce(cargs));
		return EvaluationLink::do_evaluate(as, reduct, silent);
	}

	// Like a GPN, but the entire function is declared in the
	// AtomSpace.
	if (PREDICATE_FORMULA_LINK == pntype)
		return PredicateFormulaLinkCast(pn)->apply(as, cargs, silent);

	if (DYNAMIC_FORMULA_LINK == pntype)
		return reduce_formula(pn, cargs);

	// Treat LambdaLink as if it were a PutLink -- perform
	// the beta-reduction, and evaluate the result.
	if (LAMBDA_LINK == pntype)
	{
		LambdaLinkPtr lam(LambdaLinkCast(pn));
		Handle reduct(lam->beta_reduce(cargs));
		return EvaluationLink::do_evaluate(as, reduct, silent);
	}

	// Throw a silent exception; this is called in some try..catch blocks.
	if (GROUNDED_PREDICATE_NODE == pntype)
	{
		GroundedProcedureNodePtr gpn = GroundedProcedureNodeCast(pn);
		Handle args(createLink(std::move(cargs), LIST_LINK));
		return TruthValueCast(gpn->execute(as, args, silent));
	}

	// If it's evaluatable, assume it has some free variables.
	// Use the LambdaLink to find those variables (via FreeLink)
	// and then reduce it.
	if (nameserver().isA(pntype, EVALUATABLE_LINK))
	{
		LambdaLinkPtr lam(createLambdaLink(HandleSeq({pn})));
		Handle reduct(lam->beta_reduce(cargs));
		return EvaluationLink::do_evaluate(as, reduct, silent);
	}

	if (silent)
		throw NotEvaluatableException();
	throw SyntaxException(TRACE_INFO,
			"This predicate is not evaluatable: %s", pn->to_string().c_str());
}

/// `do_eval_scratch()` -- evaluate any Atoms that can meaningfully
/// result in a fuzzy or probabilistic truth value. See description
/// for `crispy_eval_scratch()`, up above, for a general explanation.
/// This function handles miscellaneous Atoms that don't have a natural
/// interpretation in terms of crisp truth values.
///
/// If the argument is an EvaluationLink with a GPN in it, it should
/// have the following structure:
///
///     EvaluationLink
///         GroundedPredicateNode "lang: func_name"
///         ListLink
///             SomeAtom
///             OtherAtom
///
/// The `lang:` should be either `scm:` for scheme, `py:` for python,
/// or `lib:` for haskell.  This method will then invoke `func_name`
/// on the provided ListLink of arguments.
///
static TruthValuePtr tv_eval_scratch(AtomSpace* as,
                                     const Handle& evelnk,
                                     AtomSpace* scratch,
                                     bool silent,
                                     bool& try_crispy)
{
	try_crispy = false;
	Type t = evelnk->get_type();
	if (EVALUATION_LINK == t)
	{
		const HandleSeq& sna(evelnk->getOutgoingSet());

		// An ungrounded predicate evaluates to itself
		if (sna.at(0)->get_type() == PREDICATE_NODE)
		{
			// If its not in any atomspace, well, we need to have
			// it somewhere, to get an accurate TV value. We add
			// it to scratch, just in case it's not in the base as.
			if (as and as != evelnk->getAtomSpace())
				return scratch->add_atom(evelnk)->getTruthValue();
			return evelnk->getTruthValue();
		}

		HandleSeq args;
		if (LIST_LINK == sna.at(1)->get_type())
		{
			if (2 != sna.size())
				throw SyntaxException(TRACE_INFO,
					"EvaluationLink: Incorrect number of arguments, "
					"expecting 2, got %lu for:\n\t%s",
					sna.size(), evelnk->to_string().c_str());
			args = sna.at(1)->getOutgoingSet();
		}
		else
		{
			// Copy all but the first.
			// XXX Is there a more efficient way to do this copy?
			size_t sz = sna.size();
			for (size_t i=1; i<sz; i++) args.push_back(sna[i]);
		}

		// Extract the args, and run the evaluation with them.
		TruthValuePtr tvp(do_eval_with_args(scratch,
		                                    sna.at(0), args, silent));
		evelnk->setTruthValue(tvp);
		return tvp;
	}
	else if (SATISFACTION_LINK == t)
	{
		if (not is_evaluatable_sat(evelnk))
			return evelnk->evaluate(as);

		// If we are here, then we can optimize: we can evaluate
		// directly, instead of going through the pattern matcher.
		// The only reason we want to do even this much is to do
		// tail-recursion optimization, if possible.
		return EvaluationLink::do_eval_scratch(as,
		                     evelnk->getOutgoingAtom(0), scratch, silent);
	}
	else if (PUT_LINK == t)
	{
		PutLinkPtr pl(PutLinkCast(evelnk));

		// Evalating a PutLink requires three steps:
		// (1) execute the arguments, first,
		// (2) beta reduce (put arguments into body)
		// (3) evaluate the resulting body.
		Handle pvals = pl->get_arguments();
		Instantiator inst(as);
		// Step (1)
		Handle gvals(HandleCast(inst.execute(pvals, silent)));
		if (gvals != pvals)
		{
			as->add_atom(gvals);
			HandleSeq goset;
			if (pl->get_vardecl())
				goset.emplace_back(pl->get_vardecl());
			goset.emplace_back(pl->get_body());
			goset.emplace_back(gvals);
			pl = createPutLink(std::move(goset));
		}
		// Step (2)
		Handle red = HandleCast(pl->execute(as));

		// Step (3)
		return EvaluationLink::do_eval_scratch(as, red, scratch, silent);
	}
	else if (DEFINED_PREDICATE_NODE == t)
	{
		return EvaluationLink::do_eval_scratch(as,
		                       DefineLink::get_definition(evelnk),
		                       scratch, silent);
	}
	else if (DYNAMIC_FORMULA_LINK == t)
	{
		return createFormulaTruthValue(HandleSeq(evelnk->getOutgoingSet()));
	}

	else if (nameserver().isA(t, VALUE_OF_LINK))
	{
		ValuePtr pap(evelnk->execute());
		// If it's an atom, recursively evaluate.
		if (pap->is_atom())
			return EvaluationLink::do_eval_scratch(as,
			                    HandleCast(pap), scratch, silent);

		return TruthValueCast(pap);
	}
	else if (evelnk->is_evaluatable())
	{
		return evelnk->evaluate(scratch, silent);
	}
	else if ( // Links that evaluate to themselves
		nameserver().isA(t, DIRECTLY_EVALUATABLE_LINK))
	{
		return evelnk->getTruthValue();
	}

	try_crispy = true;
	return nullptr;
}

TruthValuePtr EvaluationLink::do_eval_scratch(AtomSpace* as,
                                              const Handle& evelnk,
                                              AtomSpace* scratch,
                                              bool silent)
{
	// Try the probabilistic ones first, then the crispy ones.
	bool fail;
	TruthValuePtr tvp = tv_eval_scratch(as, evelnk, scratch,
	                                    silent, fail);
	if (not fail) return tvp;

	return bool_to_tv(crispy_eval_scratch(as, evelnk, scratch, silent));
}

TruthValuePtr EvaluationLink::do_evaluate(AtomSpace* as,
                                          const Handle& evelnk,
                                          bool silent)
{
	return do_eval_scratch(as, evelnk, as, silent);
}

bool EvaluationLink::crisp_eval_scratch(AtomSpace* as,
                                        const Handle& evelnk,
                                        AtomSpace* scratch,
                                        bool silent)
{
	// Try the crispy ones first, then the probabilistic ones.
	bool fuzzy;
	bool tf = crispy_maybe(as, evelnk, scratch, silent, fuzzy);
	if (not fuzzy) return tf;

	bool fail;
	const TruthValuePtr& tvp = tv_eval_scratch(as, evelnk, scratch,
	                                           silent, fail);
	if (not fail)
		return tvp->get_mean() >= 0.5;

	throwSyntaxException(silent,
		"Either incorrect or not implemented yet. Cannot evaluate %s",
		evelnk->to_string().c_str());

	return false; // make compiler stop complaining.
}

bool EvaluationLink::crisp_evaluate(AtomSpace* as,
                                    const Handle& evelnk,
                                    bool silent)
{
	return crisp_eval_scratch(as, evelnk, as, silent);
}

DEFINE_LINK_FACTORY(EvaluationLink, EVALUATION_LINK)
