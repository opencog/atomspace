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
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/reduct/FoldLink.h>
#include <opencog/atoms/reduct/NumericFunctionLink.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/value/LinkValue.h>

#include <opencog/atomspace/AtomSpace.h>

#include "Force.h"
#include "EvaluationLink.h"

#include <cmath>

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

	if (nameserver().isA(t, LINK_VALUE))
	{
		pap = LinkValueCast(pap)->value()[0];
		t = pap->get_type();
	}

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

static ValuePtr exec_or_eval(AtomSpace* as,
                             const Handle& term,
                             AtomSpace* scratch,
                             bool silent)
{
	if (nameserver().isA(term->get_type(), EVALUATABLE_LINK))
	{
		try
		{
			if (EvaluationLink::crisp_eval_scratch(as, term, scratch, silent))
				return ValueCast(createBoolValue(true));
			return ValueCast(createBoolValue(false));
		}
		catch (const SilentException& ex)
		{
			return term;
		}
	}

	// Nothing to do, here.
	if (VALUE_SHIM_LINK == term->get_type())
		return term->execute();

	// Argh. The ValueShimLink might be buried deeper in the expression.
	// In this case, ValueShimLink::setAtomSpace() will throw a
	// RuntimeException. Catch that. Perhaps it should be changed to
	// a SilentException? Except that SilentExceptions are hard to
	// debug, because they don't set a message,
	Handle sterm;
	try
	{
		sterm = scratch->add_atom(term);
	}
	catch (const RuntimeException& ex)
	{
		return term->execute();
	}

	Instantiator inst(as);
	ValuePtr vp(inst.execute(sterm, silent));

	// If the return value is a ContainerValue, we assume that this
	// is the result of executing a MeetLink or QueryLink.
	// In this case, unwrap it, to get the "actual value".
	// This feels slightly hacky, but will do for just right now.
	if (vp->is_type(CONTAINER_VALUE))
	{
		HandleSeq hs(LinkValueCast(vp)->to_handle_seq());
		if (1 == hs.size())
			vp = hs[0];
	}
	if (vp->is_atom()) scratch->add_atom(HandleCast(vp));
	return vp;
}

/// Return true, if any Atom in the outgoing set is being used as
/// a key somewhere. Usually, the outgoing set will be just one Atom,
/// but we are prepared for anything, here.
static bool is_key(const Handle& h)
{
	for (const Handle& ho : h->getOutgoingSet())
		if (ho->isKey()) return true;

	return false;
}

/// Return true, if any Atom in the outgoing set is being used as
/// a message somewhere. Usually, the outgoing set will be just one Atom,
/// but we are prepared for anything, here.
static bool is_message(const Handle& h)
{
	for (const Handle& ho : h->getOutgoingSet())
		if (ho->isMessage()) return true;

	return false;
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
	return (*h0 == *h1a);
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

/// `crisp_eval_with_args()` -- evaluate a PredicateNode with arguments,
/// returning a boolean result.
///
/// Expects "pn" to be any actively-evaluatable predicate type.
///     Currently, this includes the GroundedPredicateNode and
///     the DefinedPredicateNode.
/// Expects "args" to be a ListLink. These arguments will be
///     substituted into the predicate.
///
/// The predicate as a whole is then evaluated and returns bool.
///
/// This is called after unwrapping EvaluationLinks of the form
///
///     EvaluationLink
///         GroundedPredicateNode "lang: func_name"
///         ListLink
///             SomeAtom
///             OtherAtom
///
/// or
///
///     EvaluationLink
///         GroundedPredicateNode "lang: func_name"
///         SomeAtom
///         OtherAtom
///
/// (Skipping the ListLink...)
///
/// The `lang:` should be either `scm:` for scheme, `py:` for python,
/// or `lib:` for c/c++ code.  This method will then invoke `func_name`
/// on the provided ListLink of arguments.
///
/// For DefinedPredicateNodes, the defintiion is looked up first.
///
static bool crisp_eval_with_args(AtomSpace* as,
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

		// If its not a LambdaLink, then I don't know what to do...
		if (LAMBDA_LINK != dtype)
			throw SyntaxException(TRACE_INFO,
				"Expecting definition to be a LambdaLink, got %s",
				defn->to_string().c_str());

		// Treat LambdaLink as if it were a PutLink -- perform
		// the beta-reduction, and evaluate the result.
		LambdaLinkPtr lam(LambdaLinkCast(defn));
		Handle reduct(lam->beta_reduce(cargs));
		return EvaluationLink::crisp_eval_scratch(as, reduct, as, silent);
	}

	// Treat LambdaLink as if it were a PutLink -- perform
	// the beta-reduction, and evaluate the result.
	if (LAMBDA_LINK == pntype)
	{
		LambdaLinkPtr lam(LambdaLinkCast(pn));
		Handle reduct(lam->beta_reduce(cargs));
		return EvaluationLink::crisp_eval_scratch(as, reduct, as, silent);
	}

	// Throw a silent exception; this is called in some try..catch blocks.
	if (GROUNDED_PREDICATE_NODE == pntype)
	{
		GroundedProcedureNodePtr gpn = GroundedProcedureNodeCast(pn);
		Handle args(createLink(std::move(cargs), LIST_LINK));
		ValuePtr result = gpn->execute_args(as, args, silent);

		// Check if result is a BoolValue and return the bool directly
		if (result->is_type(BOOL_VALUE))
		{
			BoolValuePtr bvp = BoolValueCast(result);
			std::vector<bool> bvals = bvp->value();
			if (bvals.empty())
				return false;
			// Use first boolean value
			return bvals[0];
		}
		if (result->is_type(VOID_VALUE))
			throwSyntaxException(silent, "GroundedPredicate returned VoidValue");

		throw RuntimeException(TRACE_INFO,
			"GroundedPredicates MUST return BoolValue or VoidValue; got %s",
			result->to_string().c_str());
	}

	// If it's evaluatable, assume it has some free variables.
	// Use the LambdaLink to find those variables (via FreeLink)
	// and then reduce it.
	if (nameserver().isA(pntype, EVALUATABLE_LINK))
	{
		LambdaLinkPtr lam(createLambdaLink(HandleSeq({pn})));
		Handle reduct(lam->beta_reduce(cargs));
		return EvaluationLink::crisp_eval_scratch(as, reduct, as, silent);
	}

	if (silent)
		throw NotEvaluatableException();
	throw SyntaxException(TRACE_INFO,
			"This predicate is not evaluatable: %s", pn->to_string().c_str());
}

/// `crisp_eval_scratch()` -- evaluate any Atoms that can meaningfully
/// result in a crisp-logic, binary true/false truth value.
///
/// This handles evaluation of all crisp Boolean-logic constructs including:
/// - Logical constants (TrueLink, FalseLink)
/// - Logical connectives (NotLink, AndLink, OrLink, SequentialAndLink, etc.)
/// - Comparison operations (GreaterThanLink, EqualLink, IdenticalLink, etc.)
/// - Predicates (EvaluationLink with GroundedPredicateNode, DefinedPredicateNode)
/// - Special forms (PutLink)
/// - Set operations (MemberLink, SubsetLink, ExclusiveLink)
///
/// All evaluation is done with crisp Boolean semantics - there are no
/// fuzzy or probabilistic truth values involved. Results are always
/// true or false.
///
/// The implementation here is one big case-statement. It works.
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
bool EvaluationLink::crisp_eval_scratch(AtomSpace* as,
                                        const Handle& evelnk,
                                        AtomSpace* scratch,
                                        bool silent)
{
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
		return not EvaluationLink::crisp_eval_scratch(as,
		      evelnk->getOutgoingAtom(0), scratch, silent);
	}
	if (AND_LINK == t)
	{
		for (const Handle& h : evelnk->getOutgoingSet())
		{
			bool tv = EvaluationLink::crisp_eval_scratch(as, h, scratch, silent);
			if (not tv) return false;
		}
		return true;
	}
	if (OR_LINK == t)
	{
		for (const Handle& h : evelnk->getOutgoingSet())
		{
			bool tv = EvaluationLink::crisp_eval_scratch(as, h, scratch, silent);
			if (tv) return true;
		}
		return false;
	}
	if (SEQUENTIAL_AND_LINK == t)
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
				bool tv = EvaluationLink::crisp_eval_scratch(as, oset[i], scratch, silent);
				if (not tv) return false;
			}
		} while (is_trec);
		return true;
	}
	if (SEQUENTIAL_OR_LINK == t)
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
				bool tv = EvaluationLink::crisp_eval_scratch(as, oset[i], scratch, silent);
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
	if (MEMBER_LINK == t) return member(scratch, evelnk, silent);
	if (SUBSET_LINK == t) return subset(scratch, evelnk, silent);
	if (EXCLUSIVE_LINK == t) return exclusive(scratch, evelnk, silent);
	if (IS_KEY_LINK == t) return is_key(evelnk);
	if (IS_MESSAGE_LINK == t) return is_message(evelnk);

	// -------------------------
	if (nameserver().isA(t, CRISP_OUTPUT_LINK) and
	    evelnk->is_evaluatable())
	{
		return evelnk->bevaluate(scratch, silent);
	}

	// -------------------------
	if (EVALUATION_LINK == t)
	{
		const HandleSeq& sna(evelnk->getOutgoingSet());

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
			size_t sz = sna.size();
			for (size_t i=1; i<sz; i++) args.push_back(sna[i]);
		}

		// Extract the args, and run the evaluation with them.
		return crisp_eval_with_args(scratch, sna.at(0), args, silent);
	}

	// PutLinks implement beta-reduction. This is special-cased here,
	// so that first, the beta-reduction is performed, and then the
	// result is evaluated (with the crisp evaluator).
	if (PUT_LINK == t)
	{
		PutLinkPtr pl(PutLinkCast(evelnk));
		Handle red = HandleCast(pl->execute(as));
		return EvaluationLink::crisp_eval_scratch(as, red, scratch, silent);
	}

	if (DEFINED_PREDICATE_NODE == t)
	{
		return EvaluationLink::crisp_eval_scratch(as, DefineLink::get_definition(evelnk), scratch, silent);
	}

	if (nameserver().isA(t, VALUE_OF_LINK))
	{
		ValuePtr pap(evelnk->execute(scratch));

		// There might not be a Value at the given key.
		if (nullptr == pap)
			return false;

		// If it's a BoolValue, extract the boolean directly
		if (pap->is_type(BOOL_VALUE))
		{
			BoolValuePtr bvp = BoolValueCast(pap);
			std::vector<bool> bvals = bvp->value();
			if (bvals.empty())
				return false;
			// Use first boolean value
			return bvals[0];
		}

		// If it's an atom, recursively evaluate.
		if (pap->is_atom())
			return EvaluationLink::crisp_eval_scratch(as, HandleCast(pap), scratch, silent);
	}

	throwSyntaxException(silent,
		"Either incorrect or not implemented yet. Cannot evaluate %s",
		evelnk->to_string().c_str());

	return false; // make compiler stop complaining.
}

DEFINE_LINK_FACTORY(EvaluationLink, EVALUATION_LINK)
