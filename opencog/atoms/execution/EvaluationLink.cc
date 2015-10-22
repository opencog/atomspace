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

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/SimpleTruthValue.h>
#include <opencog/atoms/NumberNode.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/reduct/FoldLink.h>
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

static Handle fold_execute(AtomSpace* as, const Handle& h)
{
	FoldLinkPtr flp(FoldLinkCast(FoldLink::factory(LinkCast(h))));
	if (NULL == flp)
		throw RuntimeException(TRACE_INFO, "Not executable!");

	return flp->execute(as);
}

// Perform a GreaterThan check
static TruthValuePtr greater(AtomSpace* as, const LinkPtr& ll)
{
	if (2 != ll->getArity())
		throw RuntimeException(TRACE_INFO,
		     "GreaterThankLink expects two arguments");
	Handle h1(ll->getOutgoingAtom(0));
	Handle h2(ll->getOutgoingAtom(1));

	// If they are not numbers, then we expect them to be something that
	// can be executed, yeilding a number.
	if (NUMBER_NODE != h1->getType())
		h1 = fold_execute(as, h1);

	if (NUMBER_NODE != h2->getType())
		h2 = fold_execute(as, h2);

	NumberNodePtr n1(NumberNodeCast(h1));
	NumberNodePtr n2(NumberNodeCast(h2));

	if (NULL == n1 or NULL == n2)
		throw RuntimeException(TRACE_INFO,
		    "Expecting c++:greater arguments to be NumberNode's!  Got:\n%s\n",
		    (h1==NULL)? "(invalid handle)" : h1->toShortString().c_str(),
		    (h2==NULL)? "(invalid handle)" : h2->toShortString().c_str());

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

static bool is_tail_rec(const Handle& thish, const Handle& tail)
{
	Type tailt = tail->getType();
	if (DEFINED_PREDICATE_NODE == tailt)
	{
		if (DefineLink::get_definition(tail) == thish)
			return true;
	}
	return false;
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
                     const Handle& evelnk, AtomSpace* scratch)
{
	Type t = evelnk->getType();
	if (EVALUATION_LINK == t)
	{
		const LinkPtr l(LinkCast(evelnk));
		const HandleSeq& sna(l->getOutgoingSet());

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
	else if (TRUE_LINK == t or FALSE_LINK == t)
	{
		// Assume that the link is wrapping something executable,
		// which we execute, but then ignore the result.
		const LinkPtr ll(LinkCast(evelnk));
		Instantiator inst(as);
		Handle result(inst.execute(ll->getOutgoingAtom(0)));
		as->add_atom(result);
		if (TRUE_LINK == t)
			return TruthValue::TRUE_TV();
		return TruthValue::FALSE_TV();
	}
	else if (SATISFACTION_LINK == t)
	{
		return satisfaction_link(as, evelnk);
	}
	else if (DEFINED_PREDICATE_NODE == t)
	{
		return do_eval_scratch(as, DefineLink::get_definition(evelnk), scratch);
	}

	// We do not want to waste CPU time printing an exception message;
	// this is supposed to be handled automatically.  Hmmm... unless
	// its a user Syntax error ....
	throw NotEvaluatableException();
	// throw SyntaxException(TRACE_INFO,
		// "Expecting to get an EvaluationLink, got %s",
		// evelnk->toString().c_str());
}

TruthValuePtr EvaluationLink::do_evaluate(AtomSpace* as, const Handle& evelnk)
{
	return do_eval_scratch(as, evelnk, as);
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
