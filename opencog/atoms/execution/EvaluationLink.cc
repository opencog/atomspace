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

#include <opencog/atoms/base/atom_types.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/NumberNode.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/PutLink.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/reduct/FoldLink.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/PythonEval.h>
#include <opencog/guile/SchemeEval.h>
#include <opencog/query/BindLinkAPI.h>
#include "EvaluationLink.h"

using namespace opencog;

EvaluationLink::EvaluationLink(const HandleSeq& oset,
                               TruthValuePtr tv,
                               AttentionValuePtr av)
    : FreeLink(EVALUATION_LINK, oset, tv, av)
{
	if ((2 != oset.size()) or
	   (LIST_LINK != oset[1]->getType()))
	{
		throw RuntimeException(TRACE_INFO,
		    "EvaluationLink must have predicate and args!");
	}
}

EvaluationLink::EvaluationLink(const Handle& schema, const Handle& args,
                               TruthValuePtr tv,
                               AttentionValuePtr av)
    : FreeLink(EVALUATION_LINK, schema, args, tv, av)
{
	if (LIST_LINK != args->getType()) {
		throw RuntimeException(TRACE_INFO,
		    "EvaluationLink must have args in a ListLink!");
	}
}

EvaluationLink::EvaluationLink(Link& l)
    : FreeLink(l)
{
	Type tscope = l.getType();
	if (EVALUATION_LINK != tscope) {
		throw RuntimeException(TRACE_INFO,
		    "Expecting an EvaluationLink");
	}
}

// Pattern matching hack. The pattern matcher returns sets of atoms;
// if that set contains a single number, then unwrap it.
static NumberNodePtr unwrap_set(Handle h)
{
	if (SET_LINK == h->getType())
	{
		LinkPtr lp(LinkCast(h));
		if (1 != lp->getArity())
			throw SyntaxException(TRACE_INFO,
				"Don't know how to do arithmetic with this: %s",
				h->toString().c_str());
		h = lp->getOutgoingAtom(0);
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
			h->toString().c_str());
	return na;
}

// Perform a GreaterThan check
static TruthValuePtr greater(AtomSpace* as, const LinkPtr& ll)
{
	const HandleSeq& oset = ll->getOutgoingSet();
	if (2 != oset.size())
		throw RuntimeException(TRACE_INFO,
		     "GreaterThankLink expects two arguments");

	Instantiator inst(as);
	Handle h1(inst.execute(oset[0]));
	Handle h2(inst.execute(oset[1]));

	NumberNodePtr n1(unwrap_set(h1));
	NumberNodePtr n2(unwrap_set(h2));

	if (n1->get_value() > n2->get_value())
		return TruthValue::TRUE_TV();
	else
		return TruthValue::FALSE_TV();
}

static TruthValuePtr equal(AtomSpace* as, const LinkPtr& ll)
{
	const HandleSeq& oset = ll->getOutgoingSet();
	if (2 != oset.size())
		throw RuntimeException(TRACE_INFO,
		     "EqualLink expects two arguments");

	Instantiator inst(as);
	Handle h0(inst.execute(oset[0]));
	Handle h1(inst.execute(oset[1]));

	if (h0 == h1)
		return TruthValue::TRUE_TV();
	else
		return TruthValue::FALSE_TV();
}

static bool is_evaluatable_sat(const Handle& satl)
{
	LinkPtr lp(LinkCast(satl));
	if (1 != lp->getArity())
		return false;

	PatternLinkPtr plp(PatternLinkCast(satl));
	if (nullptr == plp)
		plp = createPatternLink(satl);

	return 0 == plp->get_variables().varseq.size();
}

static bool is_tail_rec(const Handle& thish, const Handle& tail)
{
	if (DEFINED_PREDICATE_NODE != tail->getType())
		return false;

	Handle defn(DefineLink::get_definition(tail));
	if (defn == thish)
		return true;

	if (SATISFACTION_LINK != defn->getType())
		return false;

	if (not is_evaluatable_sat(defn))
		return false;

	LinkPtr l(LinkCast(defn));
	if (thish == l->getOutgoingAtom(0))
		return true;

	return false;
}

static void thread_eval(AtomSpace* as,
                        const Handle& evelnk, AtomSpace* scratch,
                        bool silent)
{
	EvaluationLink::do_eval_scratch(as, evelnk, scratch, silent);
}

static void thread_eval_tv(AtomSpace* as,
                     const Handle& evelnk, AtomSpace* scratch,
                     bool silent, TruthValuePtr* tv)
{
	*tv = EvaluationLink::do_eval_scratch(as, evelnk, scratch, silent);
}

/// do_evaluate -- evaluate the GroundedPredicateNode of the EvaluationLink
///
/// Expects the argument to be an EvaluationLink, which should have the
/// following structure:
///
///     EvaluationLink
///         GroundedPredicateNode "lang: func_name"
///         ListLink
///             SomeAtom
///             OtherAtom
///
/// The "lang:" should be either "scm:" for scheme, or "py:" for python.
/// This method will then invoke "func_name" on the provided ListLink
/// of arguments to the function.
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
TruthValuePtr EvaluationLink::do_eval_scratch(AtomSpace* as,
                     const Handle& evelnk, AtomSpace* scratch,
                     bool silent)
{
	Type t = evelnk->getType();
	if (EVALUATION_LINK == t)
	{
		const LinkPtr l(LinkCast(evelnk));
		const HandleSeq& sna(l->getOutgoingSet());

		if (2 != sna.size())
			throw SyntaxException(TRACE_INFO,
				"Incorrect number of arguments, expecting 2, got %lu",
				sna.size());

		// An ungrounded predicate evaluates to itself
		if (sna.at(0)->getType() == PREDICATE_NODE)
			return evelnk->getTruthValue();

		// The arguments may need to be executed...
		Instantiator inst(scratch);
		Handle args(inst.execute(sna.at(1)));

		return do_evaluate(scratch, sna.at(0), args);
	}
	else if (EQUAL_LINK == t)
	{
		return equal(scratch, LinkCast(evelnk));
	}
	else if (GREATER_THAN_LINK == t)
	{
		return greater(scratch, LinkCast(evelnk));
	}
	else if (NOT_LINK == t)
	{
		LinkPtr l(LinkCast(evelnk));
		TruthValuePtr tv(do_eval_scratch(as, l->getOutgoingAtom(0), scratch));
		return SimpleTruthValue::createTV(
		              1.0 - tv->getMean(), tv->getCount());
	}
	else if (AND_LINK == t)
	{
		LinkPtr l(LinkCast(evelnk));
		for (const Handle& h : l->getOutgoingSet())
		{
			TruthValuePtr tv(do_eval_scratch(as, h, scratch));
			if (tv->getMean() < 0.5)
				return tv;
		}
		return TruthValue::TRUE_TV();
	}
	else if (OR_LINK == t)
	{
		LinkPtr l(LinkCast(evelnk));
		for (const Handle& h : l->getOutgoingSet())
		{
			TruthValuePtr tv(do_eval_scratch(as, h, scratch));
			if (0.5 < tv->getMean())
				return tv;
		}
		return TruthValue::FALSE_TV();
	}
	else if (SEQUENTIAL_AND_LINK == t)
	{
		LinkPtr l(LinkCast(evelnk));
		const HandleSeq& oset = l->getOutgoingSet();
		size_t arity = oset.size();
		if (0 == arity) return TruthValue::TRUE_TV();

		// Is this tail-recursive? If so, then handle it.
		bool is_trec = is_tail_rec(evelnk, oset[arity-1]);
		if (is_trec) arity--;

		// Loop at least once. If tail-recurive, loop forever.
		do
		{
			for (size_t i=0; i<arity; i++)
			{
				TruthValuePtr tv(do_eval_scratch(as, oset[i], scratch));
				if (tv->getMean() < 0.5)
					return tv;
			}
		} while (is_trec);
		return TruthValue::TRUE_TV();
	}
	else if (SEQUENTIAL_OR_LINK == t)
	{
		LinkPtr l(LinkCast(evelnk));
		const HandleSeq& oset = l->getOutgoingSet();
		size_t arity = oset.size();
		if (0 == arity) return TruthValue::FALSE_TV();

		// Is this tail-recursive? If so, then handle it.
		bool is_trec = is_tail_rec(evelnk, oset[arity-1]);
		if (is_trec) arity--;

		// Loop at least once. If tail-recurive, loop forever.
		do
		{
			for (size_t i=0; i<arity; i++)
			{
				TruthValuePtr tv(do_eval_scratch(as, oset[i], scratch));
				if (0.5 < tv->getMean())
					return tv;
			}
		} while (is_trec);
		return TruthValue::FALSE_TV();
	}
	else if (JOIN_LINK == t)
	{
		LinkPtr l(LinkCast(evelnk));
		const HandleSeq& oset = l->getOutgoingSet();
		size_t arity = oset.size();
		std::vector<TruthValuePtr> tvp(arity);

		// Create a collection of joinable threads.
		std::vector<std::thread> thread_set;
		for (size_t i=0; i< arity; i++)
		{
			thread_set.push_back(std::thread(&thread_eval_tv,
				as, oset[i], scratch, silent, &tvp[i]));
		}

		// Wait for it all to come together.
		for (std::thread& t : thread_set) t.join();

		// Return the logical-AND of the returned truth values
		for (const TruthValuePtr& tv: tvp)
		{
			if (0.5 > tv->getMean())
				return tv;
		}
		return TruthValue::TRUE_TV();
	}
	else if (PARALLEL_LINK == t)
	{
		LinkPtr l(LinkCast(evelnk));

		// Create and detach threads; return immediately.
		for (const Handle& h : l->getOutgoingSet())
		{
			std::thread thr(&thread_eval, as, h, scratch, silent);
			thr.detach();
		}
		return TruthValue::TRUE_TV();
	}
	else if (TRUE_LINK == t or FALSE_LINK == t)
	{
		// Assume that the link is wrapping something executable,
		// which we execute, but then ignore the result. Well, we need
		// to put it in the (scratch) atomspace ... but we ignore the
		// TV on it. We execute for the side-effects, of course.
		// We put the result in the scratch space, presumably because
		// this is some GroundedSchemaNode calling some func with some
		// args, and its pointless to put that function+args in the
		// atomspace.
		const LinkPtr ll(LinkCast(evelnk));
		if (0 < ll->getArity())
		{
			Instantiator inst(as);
			Handle result(inst.execute(ll->getOutgoingAtom(0)));
			scratch->add_atom(result);
		}
		if (TRUE_LINK == t)
			return TruthValue::TRUE_TV();
		return TruthValue::FALSE_TV();
	}
	else if (SATISFACTION_LINK == t)
	{
		if (not is_evaluatable_sat(evelnk))
			return satisfaction_link(as, evelnk);

		// If we are here, the we can optimize: we can evaluate
		// directly, instead of going through the pattern matcher.
		// The only reason we want to do even this much is to do
		// tail-recursion optimization, if possible.
		LinkPtr l(LinkCast(evelnk));
		return do_eval_scratch(as, l->getOutgoingAtom(0), scratch);
	}
	else if (PUT_LINK == t)
	{
		PutLinkPtr pl(PutLinkCast(evelnk));
		if (nullptr == pl)
			pl = createPutLink(*LinkCast(evelnk));

		// Evalating a PutLink requires three steps:
		// (1) execute the values, first,
		// (2) beta reduce (put values into body)
		// (3) evaluate the resulting body.
		Handle pvals = pl->get_values();
		Instantiator inst(as);
		// Step (1)
		Handle gvals = inst.execute(pvals);
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
		return do_eval_scratch(as, red, scratch);
	}
	else if (DEFINED_PREDICATE_NODE == t)
	{
		return do_eval_scratch(as, DefineLink::get_definition(evelnk), scratch);
	}

	// We get exceptions here in two differet ways: (a) due to user
	// error, in which case we need to print an error, and (b) intentionally,
	// e.g. when Instantiator calls us, knowing it will get an error,
	// in which case, printing the exception message is a waste of CPU
	// time...
	//
	// DefaultPatternMatchCB.cc and also Instantiator wants to catch
	// the NotEvaluatableException thrown here.  Basically, these
	// know that they might be sending non-evaluatable atoms here, and
	// don't want to garbage up the log files with bogus errors.
	if (silent)
		throw NotEvaluatableException();

	throw SyntaxException(TRACE_INFO,
		"Expecting to get an EvaluationLink, got %s",
		evelnk->toString().c_str());
}

TruthValuePtr EvaluationLink::do_evaluate(AtomSpace* as,
                                          const Handle& evelnk,
                                          bool silent)
{
	return do_eval_scratch(as, evelnk, as, silent);
}

/// do_evaluate -- evaluate the GroundedPredicateNode of the EvaluationLink
///
/// Expects the sequence to be exactly two atoms long.
/// Expects the first handle of the sequence to be a GroundedPredicateNode
/// Expects the second handle of the sequence to be a ListLink
/// Executes the GroundedPredicateNode, supplying the second handle as argument
///
TruthValuePtr EvaluationLink::do_evaluate(AtomSpace* as, const HandleSeq& sna)
{
	if (2 != sna.size())
	{
		throw RuntimeException(TRACE_INFO,
		     "Incorrect arity for an EvaluationLink!");
	}
	return do_evaluate(as, sna[0], sna[1]);
}

/// do_evaluate -- evaluate the GroundedPredicateNode of the EvaluationLink
///
/// Expects "gsn" to be a GroundedPredicateNode
/// Expects "args" to be a ListLink
/// Executes the GroundedPredicateNode, supplying the args as argument
///
TruthValuePtr EvaluationLink::do_evaluate(AtomSpace* as,
                                    const Handle& gsn, const Handle& args)
{
	if (GROUNDED_PREDICATE_NODE != gsn->getType())
	{
		// Throw a silent exception; this is called in some try..catch blocks.
		throw NotEvaluatableException();
	}
	if (LIST_LINK != args->getType())
	{
		throw RuntimeException(TRACE_INFO, "Expecting arguments to EvaluationLink!");
	}

	// Get the schema name.
	const std::string& schema = NodeCast(gsn)->getName();
	// printf ("Grounded schema name: %s\n", schema.c_str());

	// A very special-case C++ comparison.
	// This compares two NumberNodes, by their numeric value.
	// Hard-coded in C++ for speed. (well, and for convenience ...)
	if (0 == schema.compare("c++:greater"))
	{
		return greater(as, LinkCast(args));
	}

	// A very special-case C++ comparison.
	// This compares a set of atoms, verifying that they are all different.
	// Hard-coded in C++ for speed. (well, and for convenience ...)
	if (0 == schema.compare("c++:exclusive"))
	{
		LinkPtr ll(LinkCast(args));
		Arity sz = ll->getArity();
		for (Arity i=0; i<sz-1; i++) {
			Handle h1(ll->getOutgoingAtom(i));
			for (Arity j=i+1; j<sz; j++) {
				Handle h2(ll->getOutgoingAtom(j));
				if (h1 == h2) return TruthValue::FALSE_TV();
			}
		}
		return TruthValue::TRUE_TV();
	}

	// At this point, we only run scheme and python schemas.
	if (0 == schema.compare(0,4,"scm:", 4))
	{
#ifdef HAVE_GUILE
		// Be friendly, and strip leading white-space, if any.
		size_t pos = 4;
		while (' ' == schema[pos]) pos++;

		SchemeEval* applier = SchemeEval::get_evaluator(as);
		return applier->apply_tv(schema.substr(pos), args);
#else
		throw RuntimeException(TRACE_INFO,
			 "Cannot evaluate scheme GroundedPredicateNode!");
#endif /* HAVE_GUILE */
	}

	if (0 == schema.compare(0, 3,"py:", 3))
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
			 "Cannot evaluate python GroundedPredicateNode!");
#endif /* HAVE_CYTHON */
	}

	// Unkown proceedure type.
	throw RuntimeException(TRACE_INFO,
	     "Cannot evaluate unknown GroundedPredicateNode: %s",
	      schema.c_str());
}
