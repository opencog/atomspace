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
#include <opencog/atoms/grant/DefineLink.h>
#include <opencog/atoms/scope/LambdaLink.h>
#include <opencog/atoms/scope/PutLink.h>
#include <opencog/atoms/free/FindUtils.h>
#include <opencog/atoms/execution/GroundedProcedureNode.h>
#include <opencog/atoms/value/BoolValue.h>

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

	// -------------------------
	// Handle EVALUATION_LINK first, before is_evaluatable() dispatch.
	// EvaluationLink::bevaluate() calls back into crisp_eval_scratch(),
	// so we must handle it here to avoid infinite recursion.
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

	if (evelnk->is_evaluatable())
		return evelnk->bevaluate(scratch, silent);

	// -------------------------
	// PutLinks implement beta-reduction. This is special-cased here,
	// so that first, the beta-reduction is performed, and then the
	// result is evaluated (with the crisp evaluator).
	if (PUT_LINK == t)
	{
		PutLinkPtr pl(PutLinkCast(evelnk));
		ValuePtr vp(pl->execute(as));
		if (BOOL_VALUE == vp->get_type())
		{
			BoolValuePtr bvp = BoolValueCast(vp);
			std::vector<bool> bvals = bvp->value();
			if (bvals.empty())
				return false;
			// Use first boolean value
			return bvals[0];
		}
		Handle red = HandleCast(vp);
		if (nullptr == red)
			throwSyntaxException(silent,
				"Expected Atom when evaluating %s, got %s",
				evelnk->to_string().c_str(),
				vp->to_string().c_str());
		return EvaluationLink::crisp_eval_scratch(as, red, scratch, silent);
	}

	// -------------------------
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
