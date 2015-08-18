/*
 * DefaultPatternMatchCB.cc
 *
 * Copyright (C) 2008,2009,2014,2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  February 2008
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

#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/atoms/bind/BetaRedex.h>

#include "DefaultPatternMatchCB.h"

using namespace opencog;

// Uncomment below to enable debug print
// #define DEBUG
#ifdef DEBUG
#define dbgprt(f, varargs...) printf(f, ##varargs)
#else
#define dbgprt(f, varargs...)
#endif

/* ======================================================== */

DefaultPatternMatchCB::DefaultPatternMatchCB(AtomSpace* as) :
	_classserver(classserver()),
	_temp_aspace(as),
	_instor(&_temp_aspace),
	_as(as)
{
	_connectives.insert(AND_LINK);
	_connectives.insert(OR_LINK);
	_connectives.insert(NOT_LINK);
}

void DefaultPatternMatchCB::set_pattern(const Variables& vars,
                                        const Pattern& pat)
{
	_type_restrictions = &vars.typemap;
	_dynamic = &pat.evaluatable_terms;
	_have_evaluatables = (0 < _dynamic->size());
}


/* ======================================================== */

/**
 * Called when a node in the template pattern needs to
 * be compared to a possibly matching node in the atomspace.
 * The first argument is a node from the pattern, and the
 * second is a possible solution (grounding) node from the
 * atomspace.
 *
 * Return true if the nodes match, else return false.
 * By default, the nodes must be identical.
 */
bool DefaultPatternMatchCB::node_match(const Handle& npat_h,
                                       const Handle& nsoln_h)
{
	// If equality, then a match.
	return npat_h == nsoln_h;
}

/**
 * Called when a variable in the template pattern
 * needs to be compared to a possible grounding
 * node in the atomspace. The first argument
 * is a variable from the pattern, and the second
 * is a possible grounding node from the atomspace.
 * Return true if the nodes match, else return false.
 */
bool DefaultPatternMatchCB::variable_match(const Handle& npat_h,
                                           const Handle& nsoln_h)
{
	Type pattype = npat_h->getType();

	// If the ungrounded term is not of type VariableNode, then just
	// accept the match. This allows any kind of node types to be
	// explicitly bound as variables.  However, the type VariableNode
	// gets special handling, below.
	if (pattype != VARIABLE_NODE) return true;

	// If the ungrounded term is a variable, then see if there
	// are any restrictions on the variable type.
	// If no restrictions, we are good to go.
	if (_type_restrictions->empty()) return true;

	// If we are here, there's a restriction on the grounding type.
	// Validate the node type, if needed.
	VariableTypeMap::const_iterator it = _type_restrictions->find(npat_h);
	if (it == _type_restrictions->end()) return true;

	// Is the ground-atom type in our list of allowed types?
	Type soltype = nsoln_h->getType();
	const std::set<Type> &tset = it->second;
	std::set<Type>::const_iterator allow = tset.find(soltype);
	return allow != tset.end();
}

/**
 * Called when a link in the template pattern
 * needs to be compared to a possibly matching
 * link in the atomspace. The first argument
 * is a link from the pattern, and the second
 * is a possible solution link from the atomspace.
 * Return true if the links should be compared,
 * else return false.
 *
 * By default, the search continues if the link
 * arity and the link types match.
 */
bool DefaultPatternMatchCB::link_match(const LinkPtr& lpat,
                                       const LinkPtr& lsoln)
{
	// If the pattern is exactly the same link as the proposed
	// grounding, then its a perfect match.
	if (lpat == lsoln) return true;

	// Accept all ChoiceLink's by default! We will get another shot
	// at it when the contents of the ChoiceLink are examined.
	Type pattype = lpat->getType();
	if (CHOICE_LINK == pattype) return true;

	if (lpat->getArity() != lsoln->getArity()) return false;
	Type soltype = lsoln->getType();

	// If types differ, no match
	return pattype == soltype;
}

bool DefaultPatternMatchCB::post_link_match(const LinkPtr& lpat,
                                            const LinkPtr& lgnd)
{
	if (not _have_evaluatables) return true;
	Handle hp(lpat);
	if (_dynamic->find(hp) == _dynamic->end()) return true;

	// We will find ourselves here whenever the link contain
	// a GroundedPredicateNode. In this case, execute the
	// node, and declare a match, or no match, depending
	// one how the evaluation turned out.  Its "crisp logic"
	// because we use a greater-than-half for the TV.
	// This is the same behavior as used in evaluate_term().
	TruthValuePtr tv(EvaluationLink::do_evaluate(_as, lgnd->getHandle()));
	return tv->getMean() >= 0.5;
}

/**
 * Called to accept of reject a top-level clause.
 *
 * This is straight-forward, except when the pattern is a VariableNode,
 * and the proposed grounding is evaluatable.  In this case, we want to
 * treat the grounding term as if it was a regular evaluatable clause,
 * and evaluate it, and use that to determine if the match is accepted.
 *
 * This allows a clause to implement a (pre-)condition on the match.
 * If we didn't implement this here, the user could still work around
 * this by saying (NotLink (NotLink (VariableNode $x))), thus forcing
 * the "normal" path to evaluate_sentence(). So may as well do it here.
 */
bool DefaultPatternMatchCB::clause_match(const Handle& ptrn,
                                         const Handle& grnd)
{
	// Is the pattern same as the ground?
	// if (ptrn == grnd) return false;
	// Well, in a "normal" world, it intuitively makes sense to reject
	// clauses that are grounded by themselves. In the real world, this
	// runs afoul of several unusual situations. The one we care about
	// is an evaluatable clause which contains no variables.  In this
	// case, we need to accept the match in order for a SatisfactionLink
	// to get valued correctly.
	// if (ptrn == grnd) return false;

	if (ptrn->getType() == VARIABLE_NODE and
	    grnd->getType() == EVALUATION_LINK and
	    0 < LinkCast(grnd)->getArity() and
	    LinkCast(grnd)->getOutgoingAtom(0)->getType() ==
	                                     GROUNDED_PREDICATE_NODE)
	{
		dbgprt("Evaluate the grounding clause=\n%s\n",
		        grnd->toShortString().c_str());
		// We make two awkard asumptions here: the ground term itself
		// does not contain any variables, and so does not need any
		// further grounding. This actuall seems reasonable. The second
		// assumption is that the EvaluationLink is actually evaluatable,
		// which seems reasonable, except that everything else in the
		// default callback ignores the TV on EvaluationLinks. So this
		// is kind-of schizophrenic here.  Not sure what else to do.
		_temp_aspace.clear();
		TruthValuePtr tvp(EvaluationLink::do_evaluate(&_temp_aspace, grnd));

		dbgprt("clause_match evaluation yeilded tv=%s\n", tvp->toString().c_str());

		// XXX FIXME: we are making a crsip-logic go/no-go decision
		// based on the TV strength. Perhaps something more subtle might be
		// wanted, here.
		bool relation_holds = tvp->getMean() > 0.5;
		return relation_holds;
	}
	return true;
}

/**
 * The default semantics here is to reject a match if the option
 * clauses are detected.  This is in keeping with the semantics
 * AbsentLink: a match is possible only if the indicated clauses
 * are absent!
 */
bool DefaultPatternMatchCB::optional_clause_match(const Handle& ptrn,
                                                  const Handle& grnd)
{
	if (Handle::UNDEFINED == grnd) return true;
	_optionals_present = true;
	return false;
}

/* ======================================================== */

bool DefaultPatternMatchCB::eval_term(const Handle& virt,
                                      const std::map<Handle, Handle>& gnds)
{
	// Evaluation of the link requires working with an atomspace
	// of some sort, so that the atoms can be communicated to scheme or
	// python for the actual evaluation. We don't want to put the
	// proposed grounding into the "real" atomspace, because the
	// grounding might be insane.  So we put it here. This is probably
	// not very efficient, but will do for now...

	Handle gvirt(_instor.instantiate(virt, gnds));

	dbgprt("Enter eval_term CB with virt=\n%s\n",
	        virt->toShortString().c_str());
	dbgprt("grounded by gvirt=\n%s\n",
	        gvirt->toShortString().c_str());


	// At this time, we expect all virutal links to be in one of two
	// forms: either EvaluationLink's or GreaterThanLink's.  The
	// EvaluationLinks should have the structure
	//
	//   EvaluationLink
	//       GroundedPredicateNode "scm:blah"
	//       ListLink
	//           Arg1Atom
	//           Arg2Atom
	//
	// The GreaterThanLink's should have the "obvious" structure
	//
	//   GreaterThanLink
	//       Arg1Atom
	//       Arg2Atom
	//
	// XXX TODO as discussed on the mailing list, we should perhaps first
	// see if the following can be found in the atomspace:
	//
	//   EvaluationLink
	//       PredicateNode "blah"  ; not Grounded any more, and scm: stripped
	//       ListLink
	//           Arg1Atom
	//           Arg2Atom
	//
	// If it does, we should declare a match.  If not, only then run the
	// do_evaluate callback.  Alternately, perhaps the
	// EvaluationLink::do_evaluate() method should do this ??? Its a toss-up.

	_temp_aspace.clear();
	TruthValuePtr tvp(EvaluationLink::do_evaluate(&_temp_aspace, gvirt));

	dbgprt("eval_term evaluation yeilded tv=%s\n", tvp->toString().c_str());

	// XXX FIXME: we are making a crsip-logic go/no-go decision
	// based on the TV strength. Perhaps something more subtle might be
	// wanted, here.
	bool relation_holds = tvp->getMean() > 0.5;
	return relation_holds;
}

/* ======================================================== */

/**
 * This implements the evaluation of a classical boolean-logic
 * "sentence": a well-formed formula with no free variables,
 * having a crisp true/false truth value.  Here, "top" holds
 * the sentence (with variables), 'gnds' holds the bindings of
 * variables to values.
 */
bool DefaultPatternMatchCB::eval_sentence(const Handle& top,
                              const std::map<Handle, Handle>& gnds)
{
	dbgprt("Enter eval_sentence CB with top=\n%s\n",
	        top->toShortString().c_str());

	if (top->getType() == VARIABLE_NODE)
	{
		return eval_term(top, gnds);
	}

	LinkPtr ltop(LinkCast(top));
	if (NULL == ltop)
		throw InvalidParamException(TRACE_INFO,
	            "Not expecting a Node, here %s\n",
	            top->toShortString().c_str());

	const HandleSeq& oset = ltop->getOutgoingSet();
	if (0 == oset.size())
		throw InvalidParamException(TRACE_INFO,
		   "Expecting logical connective to have at least one child!");

	Type term_type = top->getType();
	if (OR_LINK == term_type)
	{
		for (const Handle& h : oset)
			if (eval_sentence(h, gnds)) return true;

		return false;
	}
	else if (AND_LINK == term_type)
	{
		for (const Handle& h : oset)
			if (not eval_sentence(h, gnds)) return false;

		return true;
	}
	else if (NOT_LINK == term_type)
	{
		if (1 != oset.size())
			throw InvalidParamException(TRACE_INFO,
			            "NotLink can have only one child!");

		return not eval_sentence(oset[0], gnds);
	}
	else if (EVALUATION_LINK == term_type or
	         _classserver.isA(term_type, VIRTUAL_LINK))
	{
		return eval_term(top, gnds);
	}
	throw InvalidParamException(TRACE_INFO,
	            "Unknown logical connective %s\n",
	            top->toShortString().c_str());
}

/* ===================== END OF FILE ===================== */
