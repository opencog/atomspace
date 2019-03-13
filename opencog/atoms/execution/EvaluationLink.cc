/*
 * opencog/atoms/execution/EvaluationLink.cc
 *
 * Copyright (C) 2009, 2013, 2014, 2015 Linas Vepstas
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

#include <thread>

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/core/PutLink.h>
#include <opencog/atoms/core/TruthValueOfLink.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/reduct/FoldLink.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/PythonEval.h>
#include <opencog/guile/SchemeEval.h>

#include "DLScheme.h"
#include "Force.h"
#include "EvaluationLink.h"
#include "LibraryManager.h"

using namespace opencog;

EvaluationLink::EvaluationLink(const HandleSeq& oset, Type t)
    : FreeLink(oset, t)
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

EvaluationLink::EvaluationLink(const Link& l)
    : FreeLink(l)
{
	Type tscope = l.get_type();
	if (EVALUATION_LINK != tscope)
		throw RuntimeException(TRACE_INFO,
		    "Expecting an EvaluationLink");
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
void throwSyntaxException(bool silent, const char* message...)
{
	if (silent)
		throw NotEvaluatableException();
	va_list args;
	va_start(args, message);
	throw SyntaxException(TRACE_INFO, message, args);
	va_end(args);
}

/// Pattern matching hack. The pattern matcher returns sets of atoms;
/// if that set contains a single number, then unwrap it.
/// See issue #1502 which proposes to eliminate this SetLink hack.
static NumberNodePtr unwrap_set(Handle h)
{
	if (SET_LINK == h->get_type())
	{
		if (1 != h->get_arity())
			throw SyntaxException(TRACE_INFO,
				"Don't know how to do arithmetic with this: %s",
				h->to_string().c_str());
		h = h->getOutgoingAtom(0);
	}

	NumberNodePtr na(NumberNodeCast(h));
	if (nullptr == na)
	{
		NodePtr np(NodeCast(h));
		if (np) na = createNumberNode(*np);
	}

	if (nullptr == na)
		throw SyntaxException(TRACE_INFO,
			"Don't know how to compare this: %s",
			h->to_string().c_str());
	return na;
}

/// Extract a single floating-point double out of a value expected to
/// contain a number.
static double get_numeric_value(const ValuePtr& pap, bool silent)
{
	Type t = pap->get_type();
	if (NUMBER_NODE == t or SET_LINK == t)
	{
		NumberNodePtr n(unwrap_set(HandleCast(pap)));
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

	Instantiator inst(as);
	ValuePtr pap0(inst.execute(oset[0]));
	ValuePtr pap1(inst.execute(oset[1]));

	double v0 = get_numeric_value(pap0, silent);
	double v1 = get_numeric_value(pap1, silent);

	return (v0 > v1);
}

/// Check for syntactic equality
static bool identical(const Handle& h)
{
	const HandleSeq& oset = h->getOutgoingSet();
	if (2 != oset.size())
		throw SyntaxException(TRACE_INFO,
		     "IdenticalLink expects two arguments");

	return (oset[0] == oset[1]);
}

/// Check for semantic equality
static bool equal(AtomSpace* as, const Handle& h, bool silent)
{
	const HandleSeq& oset = h->getOutgoingSet();
	if (2 != oset.size())
		throw SyntaxException(TRACE_INFO,
		     "EqualLink expects two arguments");

	Instantiator inst(as);
	Handle h0(HandleCast(inst.execute(oset[0], silent)));
	Handle h1(HandleCast(inst.execute(oset[1], silent)));

	return (h0 == h1);
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

	// Not strictly equal. Are they alpha convertable?
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

static void thread_eval(AtomSpace* as,
                        const Handle& evelnk, AtomSpace* scratch,
                        bool silent)
{
	try
	{
		EvaluationLink::do_eval_scratch(as, evelnk, scratch, silent);
	}
	catch (const std::exception& ex)
	{
		logger().warn("Caught exception in thread:\n%s", ex.what());
	}
}

static void thread_eval_tv(AtomSpace* as,
                           const Handle& evelnk, AtomSpace* scratch,
                           bool silent, TruthValuePtr* tv,
                           std::exception_ptr* returned_ex)
{
	try
	{
		*tv = EvaluationLink::do_eval_scratch(as, evelnk, scratch, silent);
	}
	catch (const std::exception& ex)
	{
		*returned_ex = std::current_exception();
	}
}

static TruthValuePtr bool_to_tv(bool truf)
{
	if (truf) return TruthValue::TRUE_TV();
	return TruthValue::FALSE_TV();
}


/// `crisp_eval_sratch()` -- evaluate any Atoms that can meaningfully
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
static bool crisp_eval_scratch(AtomSpace* as,
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
		// These cannot be discarded. This is explictly tested by
		// SequenceUTest::test_or_put().
		if (0 < evelnk->get_arity())
		{
			const Handle& term = evelnk->getOutgoingAtom(0);
			if (nameserver().isA(term->get_type(), EVALUATABLE_LINK))
			{
				EvaluationLink::do_eval_scratch(as, term, scratch, silent);
			}
			else
			{
				Instantiator inst(as);
				Handle result(HandleCast(inst.execute(term, silent)));
				if (result) scratch->add_atom(result);
			}
		}
		if (TRUE_LINK == t) return true;
		return false;
	}

	// -------------------------
	// Crisp-binary-valued Boolean Logical connectives
	if (NOT_LINK == t)
	{
		return not crisp_eval_scratch(as,
		      evelnk->getOutgoingAtom(0), scratch, silent);
	}
	else if (AND_LINK == t)
	{
		for (const Handle& h : evelnk->getOutgoingSet())
		{
			bool tv = crisp_eval_scratch(as, h, scratch, silent);
			if (not tv) return false;
		}
		return true;
	}
	else if (OR_LINK == t)
	{
		for (const Handle& h : evelnk->getOutgoingSet())
		{
			bool tv = crisp_eval_scratch(as, h, scratch, silent);
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
				bool tv = crisp_eval_scratch(as, oset[i], scratch, silent);
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
				bool tv = crisp_eval_scratch(as, oset[i], scratch, silent);
				if (tv) return true;
			}
		} while (is_trec);
		return false;
	}

	// -------------------------
	// Arity-two relations
	if (IDENTICAL_LINK == t)
	{
		return identical(evelnk);
	}
	else if (EQUAL_LINK == t)
	{
		return equal(scratch, evelnk, silent);
	}
	else if (ALPHA_EQUAL_LINK == t)
	{
		return alpha_equal(scratch, evelnk, silent);
	}
	else if (GREATER_THAN_LINK == t)
	{
		return greater(scratch, evelnk, silent);
	}

	// -------------------------
	// Multi-threading primitives
	if (JOIN_LINK == t)
	{
		const HandleSeq& oset = evelnk->getOutgoingSet();
		size_t arity = oset.size();
		std::vector<TruthValuePtr> tvp(arity);

		// Create a collection of joinable threads.
		std::vector<std::thread> thread_set;
		std::exception_ptr ex;
		for (size_t i=0; i<arity; i++)
		{
			thread_set.push_back(std::thread(&thread_eval_tv,
				as, oset[i], scratch, silent, &tvp[i], &ex));
		}

		// Wait for it all to come together.
		for (std::thread& t : thread_set) t.join();

		// Were there any exceptions? If so, rethrow.
		if (ex) std::rethrow_exception(ex);

		// Return the logical-AND of the returned truth values
		for (const TruthValuePtr& tv: tvp)
		{
			if (0.5 > tv->get_mean())
				return false;
		}
		return true;
	}
	else if (PARALLEL_LINK == t)
	{
		// Create and detach threads; return immediately.
		for (const Handle& h : evelnk->getOutgoingSet())
		{
			std::thread thr(&thread_eval, as, h, scratch, silent);
			thr.detach();
		}
		return true;
	}

	// A handful of link types that should be auto-converted into
	// crisp truth values.  (SatisfactinLink is already crisp; but
	// the current API does not allow it to report that. XXX FIXME).
	if (EVALUATION_LINK == t or
	    SATISFACTION_LINK == t or
	    DEFINED_PREDICATE_NODE == t)
	{
		TruthValuePtr tv(EvaluationLink::do_eval_scratch(as,
		                evelnk, scratch, silent));
		if (0.5 < tv->get_mean()) return true;
		return false;
	}

	throwSyntaxException(silent,
		"Either incorrect or not implemented yet. Cannot evaluate %s",
		evelnk->to_string().c_str());

	return false;
}

/// Evaluate a formula defined by a PREDICATE_FORMULA_LINK
static TruthValuePtr eval_formula(const Handle& predform,
                                  const HandleSeq& cargs)
{
	// Collect up two floating point values.
	std::vector<double> nums;
	for (const Handle& h: predform->getOutgoingSet())
	{
		// An ordinary number.
		if (NUMBER_NODE == h->get_type())
		{
			nums.push_back(NumberNodeCast(h)->get_value());
			continue;
		}

		// In case the user wanted to wrap everything in a
		// LambdaLink. I don't understand why this is needed,
		// but it seems to make some people feel better, so
		// we support it.
		Handle flh(h);
		if (LAMBDA_LINK == h->get_type())
		{
			// Set flh and fall through, where it is executed.
			flh = LambdaLinkCast(h)->beta_reduce(cargs);
		}

		// At this point, we expect a FunctionLink of some kind.
		if (not nameserver().isA(flh->get_type(), FUNCTION_LINK))
			throw SyntaxException(TRACE_INFO, "Expecting a FunctionLink");

		// If the FunctionLink has free variables in it,
		// reduce them with the provided arguments.
		FunctionLinkPtr flp(FunctionLinkCast(flh));
		const FreeVariables& fvars = flp->get_vars();
		if (not fvars.empty())
		{
			flh = fvars.substitute_nocheck(flh, cargs);
		}

		// Expecting a FunctionLink without variables.
		ValuePtr v(flh->execute());
		FloatValuePtr fv(FloatValueCast(v));
		nums.push_back(fv->value()[0]);
	}

	// XXX FIXME; if we are given more than two floats, then
	// perhaps we should create some other kind of TruthValue?
	// Maybe a distributional one ?? Or a CountTV ??
	return createSimpleTruthValue(nums);
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
		{
			return eval_formula(defn, cargs);
		}

		// If its not a LambdaLink, then I don't know what to do...
		if (LAMBDA_LINK != dtype)
			throw SyntaxException(TRACE_INFO,
				"Expecting definition to be a LambdaLink, got %s",
				defn->to_string().c_str());

		// Treat LambdaLink as if it were a PutLink -- perform
		// the beta-reduction, and evaluate the result.
		LambdaLinkPtr lam(LambdaLinkCast(defn));
		Handle reduct = lam->beta_reduce(cargs);
		return EvaluationLink::do_evaluate(as, reduct, silent);
	}

	// Like a GPN, but the entire function is declared in the
	// AtomSpace.
	if (PREDICATE_FORMULA_LINK == pntype)
	{
		return eval_formula(pn, cargs);
	}

	if (GROUNDED_PREDICATE_NODE != pntype)
	{
		// Throw a silent exception; this is called in some try..catch blocks.
		throw NotEvaluatableException();
	}

	// Force execution of the arguments. We have to do this, because
	// the user-defined functions are black-boxes, and cannot be trusted
	// to do lazy execution correctly. Right now, forcing is the policy.
	// We could add "scm-lazy:" and "py-lazy:" URI's for user-defined
	// functions smart enough to do lazy evaluation.
	Handle lh(createLink(cargs, LIST_LINK));
	Handle args(force_execute(as, lh, silent));

	// Get the schema name.
	const std::string& schema = pn->get_name();
	// printf ("Grounded schema name: %s\n", schema.c_str());

	// A very special-case C++ comparison.
	// This compares two NumberNodes, by their numeric value.
	// Hard-coded in C++ for speed. (well, and for convenience ...)
	if (0 == schema.compare("c++:greater"))
	{
		return bool_to_tv(greater(as, args, silent));
	}

	// A very special-case C++ comparison.
	// This compares a set of atoms, verifying that they are all different.
	// Hard-coded in C++ for speed. (well, and for convenience ...)
	if (0 == schema.compare("c++:exclusive"))
	{
		Arity sz = args->get_arity();
		for (Arity i=0; i<sz-1; i++) {
			Handle h1(args->getOutgoingAtom(i));
			for (Arity j=i+1; j<sz; j++) {
				Handle h2(args->getOutgoingAtom(j));
				if (h1 == h2) return TruthValue::FALSE_TV();
			}
		}
		return TruthValue::TRUE_TV();
	}

	// At this point, we only run scheme and python schemas.
	if (0 == schema.compare(0, 4, "scm:", 4))
	{
#ifdef HAVE_GUILE
		// Be friendly, and strip leading white-space, if any.
		size_t pos = 4;
		while (' ' == schema[pos]) pos++;

		SchemeEval* applier = get_evaluator_for_scheme(as);
		return applier->apply_tv(schema.substr(pos), args);
#else
		throw RuntimeException(TRACE_INFO,
			"This binary does not have scheme support in it; "
			"Cannot evaluate scheme GroundedPredicateNode!");
#endif /* HAVE_GUILE */
	}

	if (0 == schema.compare(0, 3, "py:", 3))
	{
#ifdef HAVE_CYTHON
		// Be friendly, and strip leading white-space, if any.
		size_t pos = 3;
		while (' ' == schema[pos]) pos++;

		// Be sure to specify the atomspace in which to work!
		PythonEval &applier = PythonEval::instance();
		return applier.apply_tv(as, schema.substr(pos), args);
#else
		throw RuntimeException(TRACE_INFO,
			"This binary does not have python support in it; "
			"Cannot evaluate python GroundedPredicateNode!");
#endif /* HAVE_CYTHON */
	}

	// Generic shared-library foreign-function interface.
	// Currently used only by the Haskell bindings.
	//
	// Extract the language, library and function
	std::string lang, lib, fun;
	LibraryManager::lang_lib_fun(schema, lang, lib, fun);
	if (lang == "lib")
	{
		void* sym = LibraryManager::getFunc(lib,fun);

		// Convert the void* pointer to the correct function type.
		TruthValuePtr* (*func)(AtomSpace*, Handle*);
		func = reinterpret_cast<TruthValuePtr* (*)(AtomSpace *, Handle*)>(sym);

		// Evaluate the predicate
		TruthValuePtr* res = func(as, &args);
		TruthValuePtr result;
		if(res != NULL)
		{
			result = *res;
			free(res);
		}

		if (nullptr == result)
			throwSyntaxException(silent,
			        "Invalid return value from predicate %s\nArgs: %s",
			        pn->to_string().c_str(),
			        oc_to_string(cargs).c_str());

		return result;
	}

	// Unkown proceedure type.
	throw RuntimeException(TRACE_INFO,
	     "Cannot evaluate unknown GroundedPredicateNode: %s",
	      schema.c_str());
}

/// `do_eval_scratch()` -- evaluate any Atoms that can meaningfully
/// result in a fuzzy or probabilistic truth value. See description
/// for `crisp_eval_scratch()`, up above, for a general explanation.
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
TruthValuePtr EvaluationLink::do_eval_scratch(AtomSpace* as,
                                              const Handle& evelnk,
                                              AtomSpace* scratch,
                                              bool silent)
{
	Type t = evelnk->get_type();
	if (EVALUATION_LINK == t)
	{
		const HandleSeq& sna(evelnk->getOutgoingSet());

		// An ungrounded predicate evaluates to itself
		if (sna.at(0)->get_type() == PREDICATE_NODE)
			return evelnk->getTruthValue();

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
		return do_eval_scratch(as, evelnk->getOutgoingAtom(0), scratch, silent);
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
			pl = createPutLink(goset);
		}
		// Step (2)
		Handle red = pl->reduce();

		// Step (3)
		return do_eval_scratch(as, red, scratch, silent);
	}
	else if (DEFINED_PREDICATE_NODE == t)
	{
		return do_eval_scratch(as, DefineLink::get_definition(evelnk),
		                       scratch, silent);
	}
	else if (// Links that evaluate to themselves
		INHERITANCE_LINK == t or
		IMPLICATION_LINK == t or
		EXECUTION_LINK == t
		)
	{
		return evelnk->getTruthValue();
	}
	else if (PREDICATE_FORMULA_LINK == t)
	{
		// A shortened, argument-free version of eval_formula()
		std::vector<double> nums;
		for (const Handle& h: evelnk->getOutgoingSet())
		{
			if (NUMBER_NODE == h->get_type())
			{
				nums.push_back(NumberNodeCast(h)->get_value());
				continue;
			}

			if (not nameserver().isA(h->get_type(), FUNCTION_LINK))
				throw SyntaxException(TRACE_INFO, "Expecting a FunctionLink");

			ValuePtr v(h->execute());
			FloatValuePtr fv(FloatValueCast(v));
			nums.push_back(fv->value().at(0));
		}
		return createSimpleTruthValue(nums);
	}
	else if (TRUTH_VALUE_OF_LINK == t)
	{
		// If the truth value of the link is being requested,
		// then ... compute the truth value, on the fly!
		Handle ofatom = evelnk->getOutgoingAtom(0);
		TruthValuePtr tvp(EvaluationLink::do_eval_scratch(as,
		                    ofatom, scratch, silent));

		// Cache the computed truth value...
		// XXX FIXME: is this a good idea, or not?
		evelnk->setTruthValue(tvp);
		return tvp;
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

	return bool_to_tv(crisp_eval_scratch(as, evelnk, scratch, silent));
}

TruthValuePtr EvaluationLink::do_evaluate(AtomSpace* as,
                                          const Handle& evelnk,
                                          bool silent)
{
	return do_eval_scratch(as, evelnk, as, silent);
}

DEFINE_LINK_FACTORY(EvaluationLink, EVALUATION_LINK)
