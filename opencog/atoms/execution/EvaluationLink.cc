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
bool EvaluationLink::bevaluate(AtomSpace* as, bool silent)
{
	Handle pn(_outgoing.at(0));
	Type pntype = pn->get_type();

	// Allow recursive definitions. This can be handy.
	while (DEFINED_PREDICATE_NODE == pntype)
	{
		pn = DefineLink::get_definition(pn);
		pntype = pn->get_type();
	}

	// Sanity check.
	if (LIST_LINK == _outgoing.at(1)->get_type() and
		(2 != _outgoing.size()))
	{
		throw SyntaxException(TRACE_INFO,
			"EvaluationLink: Incorrect number of arguments, "
			"expecting 2, got %lu for:\n\t%s",
			_outgoing.size(), to_string().c_str());
	}

	// Throw a silent exception; this is called in some try..catch blocks.
	if (GROUNDED_PREDICATE_NODE == pntype)
	{
		GroundedProcedureNodePtr gpn = GroundedProcedureNodeCast(pn);

		ValuePtr result;
		if (LIST_LINK == _outgoing.at(1)->get_type())
			result = gpn->execute_args(as, _outgoing.at(1), silent);
		else
		{
			HandleSeq args(_outgoing.begin()+1, _outgoing.end());
			Handle argl(createLink(std::move(args), LIST_LINK));
			result = gpn->execute_args(as, argl, silent);
		}

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

	HandleSeq cargs;
	if (LIST_LINK == _outgoing.at(1)->get_type())
		cargs = _outgoing.at(1)->getOutgoingSet();
	else
	{
		cargs.reserve(_outgoing.size() - 1);
		cargs.assign(_outgoing.begin()+1, _outgoing.end());
	}

	// Treat LambdaLink as if it were a PutLink -- perform
	// the beta-reduction, and evaluate the result.
	if (LAMBDA_LINK == pntype)
	{
		LambdaLinkPtr lam(LambdaLinkCast(pn));
		Handle reduct(lam->beta_reduce(cargs));
		return reduct->bevaluate(as, silent);
	}

	// If it's evaluatable, assume it has some free variables.
	// Use the LambdaLink to find those variables (via FreeLink)
	// and then reduce it.
	if (nameserver().isA(pntype, EVALUATABLE_LINK))
	{
		LambdaLinkPtr lam(createLambdaLink(HandleSeq({pn})));
		Handle reduct(lam->beta_reduce(cargs));
		return reduct->bevaluate(as, silent);
	}

	throwSyntaxException(silent,
		"Not evaluatable: %s", to_string().c_str());
}

DEFINE_LINK_FACTORY(EvaluationLink, EVALUATION_LINK)
