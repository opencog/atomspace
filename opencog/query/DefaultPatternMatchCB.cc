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

#include <opencog/util/Logger.h>

#include <opencog/atoms/core/StateLink.h>
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atomutils/FindUtils.h>

#include "DefaultPatternMatchCB.h"

using namespace opencog;

/* ======================================================== */

DefaultPatternMatchCB::DefaultPatternMatchCB(AtomSpace* as) :
	_classserver(classserver()),
	_temp_aspace(as),
	_instor(&_temp_aspace),
	_as(as)
{
	_connectives.insert(SEQUENTIAL_AND_LINK);
	_connectives.insert(SEQUENTIAL_OR_LINK);
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
	_have_variables = (0 < vars.varseq.size());
	_pattern_body = pat.body;
	_globs = &pat.globby_terms;
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
	if (VARIABLE_NODE != pattype and GLOB_NODE != pattype) return true;

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

	// If types differ, no match
	Type soltype = lsoln->getType();
	if (pattype != soltype) return false;

	// Reject mis-sized compares, unless the pattern has a glob in it.
	if (0 == _globs->count(lpat->getHandle()))
	{ 
		if (lpat->getArity() != lsoln->getArity()) return false;
	}
	else
	{
		if (lpat->getArity() > lsoln->getArity()) return false;
	}

	// No reason to reject; proceed with the compare.
	return true;
}

bool DefaultPatternMatchCB::post_link_match(const LinkPtr& lpat,
                                            const LinkPtr& lgnd)
{
	// A temp hack until we get around to implementing executable
	// terms in the search pattern. Viz, an executable term is anything
	// that should be executed before the match is made... in this case,
	// StateLinks can have only one closed-term value ...
	if (STATE_LINK == lpat->getType())
	{
		if (StateLink::get_state(lpat->getOutgoingAtom(0)) !=
		    lgnd->getOutgoingAtom(1))
			return false;
		return true;
	}

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
	// to get valued correctly. Tested in SequenceUTest.
	// if (ptrn == grnd) return false;

	// This if-statement handles the case given in the callback description.
	// It is tested by EvaluationUTest.
	if (ptrn->getType() == VARIABLE_NODE and
	    grnd->getType() == EVALUATION_LINK and
	    0 < LinkCast(grnd)->getArity() and
	    LinkCast(grnd)->getOutgoingAtom(0)->getType() ==
	                                     GROUNDED_PREDICATE_NODE)
	{
		LAZY_LOG_FINE << "Evaluate the grounding clause=" << std::endl
		              << grnd->toShortString() << std::endl;

		// We make two awkard asumptions here: the ground term itself
		// does not contain any variables, and so does not need any
		// further grounding. This actually seems reasonable. The second
		// assumption is that the EvaluationLink is actually evaluatable,
		// which seems reasonable, except that everything else in the
		// default callback ignores the TV on EvaluationLinks. So this
		// is kind-of schizophrenic here.  Not sure what else to do.
		_temp_aspace.clear();
		TruthValuePtr tvp(EvaluationLink::do_eval_scratch(_as, grnd, &_temp_aspace));

		LAZY_LOG_FINE << "Clause_match evaluation yeilded tv"
		              << std::endl << tvp->toString() << std::endl;

		// XXX FIXME: we are making a crisp-logic go/no-go decision
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
	if (nullptr == grnd) return true;
	_optionals_present = true;
	return false;
}

/* ======================================================== */

IncomingSet DefaultPatternMatchCB::get_incoming_set(const Handle& h)
{
	return h->getIncomingSet(_as);
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

	LAZY_LOG_FINE << "Enter eval_term CB with virt=" << std::endl
	              << virt->toShortString() << std::endl;
	LAZY_LOG_FINE << "Grounded by gvirt=" << std::endl
	              << gvirt->toShortString() << std::endl;

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

	TruthValuePtr tvp;
	// The instantiator would have taken care of expanding out
	// and executing any FunctionLinks and the like.  Just use
	// the TV value on the resulting atom.
	//
	// However, we also want to have a side-effect: the result of
	// exeuctng one of these things should be placed into the atomspace.
	Type vty = virt->getType();
	if (EXECUTION_OUTPUT_LINK == vty or
	    DEFINED_SCHEMA_NODE == vty or
	    _classserver.isA(vty, FUNCTION_LINK))
	{
		gvirt = _as->add_atom(gvirt);
		tvp = gvirt->getTruthValue();
	}
	else
	{
		_temp_aspace.clear();
		try
		{
			tvp = EvaluationLink::do_eval_scratch(_as, gvirt, &_temp_aspace);
		}
		catch (const NotEvaluatableException& ex)
		{
			// The do_evaluate() bove can throw if its given ungrounded
			// expressions. It can be given ungrounded expressions if
			// no grounding was found, and a final pass, run by the
			// search_finished() callback, puts us here. So handle this
			// case gracefully.
			return false;
		}
	}

	// Avoid null-pointer dereference if user specified a bogus evaluation.
	// i.e. an evaluation that failed to return a TV.
	if (NULL == tvp)
		throw InvalidParamException(TRACE_INFO,
	            "Expecting a TruthValue for an evaluatable link: %s\n",
	            gvirt->toShortString().c_str());

	LAZY_LOG_FINE << "Eval_term evaluation yeilded tv="
	              << tvp->toString() << std::endl;

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
	LAZY_LOG_FINE << "Enter eval_sentence CB with top=" << std::endl
	              << top->toShortString() << std::endl;

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
	if (OR_LINK == term_type or SEQUENTIAL_OR_LINK == term_type)
	{
		for (const Handle& h : oset)
			if (eval_sentence(h, gnds)) return true;

		return false;
	}
	else if (AND_LINK == term_type or SEQUENTIAL_AND_LINK == term_type)
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
	else if (PRESENT_LINK == term_type)
	{
		// If *every* clause in the PresentLink has been grounded,
		// then return true.  That is, PresentLink behaves like an
		// AndLink for term-presence.  The other behavior "if some
		// clause is present" is implemented by ChoiceLink.
		for (const Handle& h : oset)
		{
			try
			{
				Handle g = gnds.at(h);
			}
			catch (...)
			{
				return false;
			}
		}
		return true;
	}
	else if (ABSENT_LINK == term_type)
	{
		// If *any* clause in the AbsentLink has been grounded, then
		// return false.  That is, AbsentLink behaves like an AndLink
		// for term-absence.  Note that this conflicts with
		// PatternLink::extract_optionals(), which insists on an arity
		// of one. Viz "must all be absent"? or "if any are absent"?
		//
		// AbsentLink is same as NotLink PresentLink.
		for (const Handle& h : oset)
		{
			try
			{
				Handle g = gnds.at(h);
			}
			catch (...)
			{
				// If no grounding, that,s good, try the next one.
				continue;
			}
			// If we are here, a grounding was found; that's bad.
			return false;
		}
		return true;
	}
	else if (CHOICE_LINK == term_type)
	{
		// If *some* clause in the ChoiceLink has been grounded,
		// then return true.  That is, ChoiceLink behaves like an
		// OrLink for term-presence.  The other behavior "all clauses
		// must be present" is implemented by PresentLink.
		//
		// XXX ... This might be buggy; I'm confused. Deep in the bowels
		// of the pattern matcher, we make an explicit promise to explore
		// all possible choices.  Here, we are making no such promise;
		// instead, we're just responding to what higher layers have
		// determined.  Did those higher layers actually explore all
		// possibilities?  And if they failed to do so, can we even do
		// anything about that here? Seems like we can't do anything...
		for (const Handle& h : oset)
		{
			try
			{
				Handle g = gnds.at(h);
			}
			catch (...)
			{
				continue;
			}
			return true;
		}
		return false;
	}

	// --------------------------------------------------------
	// If we are here, then what we have is some atom that is not
	// normally "truth-valued". We can do one of three things:
	// a) Throw an exception and complain.
	// b) Invent a new link type: GetTruthValueLink, that 'returns'
	//    the TV of the atom that it wraps.
	// c) Do the above, without inventing a new link type.
	// The below implements choice (c): i.e. it gets the TV of this
	// atom, and checks to see if it is greater than 0.5 or not.
	//
	// There are several minor issues: 1) we need to check the TV
	// of the grounded atom, not the TV of the pattern, and 2) if
	// the atom is executable, we need to execute it.
	try
	{
		Handle g = gnds.at(top);
		TruthValuePtr tvp(g->getTruthValue());
		LAZY_LOG_FINE << "Non-logical atom has tv="
		              << tvp->toString() << std::endl;
		// XXX FIXME: we are making a crsip-logic go/no-go decision
		// based on the TV strength. Perhaps something more subtle might be
		// wanted, here.
		bool relation_holds = tvp->getMean() > 0.5;
		return relation_holds;
	}
	catch (...) {}

	// If it's not grounded, then perhaps its executable.
	return eval_term(top, gnds);
}

/* ===================== END OF FILE ===================== */
