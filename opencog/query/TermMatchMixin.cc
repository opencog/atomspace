/*
 * TermMatchMixin.cc
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

#include <opencog/util/algorithm.h>
#include <opencog/util/Logger.h>

#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/core/StateLink.h>
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/execution/Instantiator.h>

#include "TermMatchMixin.h"

using namespace opencog;

// #define QDEBUG 1
#ifdef QDEBUG
#define DO_LOG(STUFF) STUFF
#else
#define DO_LOG(STUFF)
#endif

/* ======================================================== */
// Cache for temp (transient) atomsapaces.  The evaluation of
// expressions during pattern matching requires having a temporary
// atomspace, treated as a scratch space to hold temporary results.
// These are then discarded, after the match is confirmed or denied.
// The issue is that creating an atomspace is CPU-intensive, so its
// cheaper to just have a cache of empty atomspaces, hanging around,
// and ready to go. The code in this section implements this.

const bool TRANSIENT_SPACE = true;
const int MAX_CACHED_TRANSIENTS = 8;

// Allocated storage for the transient atomspace cache static variables.
std::mutex TermMatchMixin::s_transient_cache_mutex;
std::vector<AtomSpace*> TermMatchMixin::s_transient_cache;

AtomSpace* TermMatchMixin::grab_transient_atomspace(AtomSpace* parent)
{
	AtomSpace* transient_atomspace = nullptr;

	// See if the cache has one...
	if (s_transient_cache.size() > 0)
	{
		// Grab the mutex lock.
		std::unique_lock<std::mutex> cache_lock(s_transient_cache_mutex);

		// Check to make sure the cache still has one now that we have
		// the mutex.
		if (s_transient_cache.size() > 0)
		{
			// Pop the last transient atomspace off the cache stack.
			transient_atomspace = s_transient_cache.back();
			s_transient_cache.pop_back();

			// Ready it for the new parent atomspace.
			transient_atomspace->ready_transient(parent);
		}
	}

	// If we didn't get one from the cache, then create a new one.
	if (!transient_atomspace)
		transient_atomspace = new AtomSpace(parent, TRANSIENT_SPACE);

	return transient_atomspace;
}

void TermMatchMixin::release_transient_atomspace(AtomSpace* atomspace)
{
	bool atomspace_cached = false;

	// If the cache is not full...
	if (s_transient_cache.size() < MAX_CACHED_TRANSIENTS)
	{
		// Grab the mutex lock.
		std::unique_lock<std::mutex> cache_lock(s_transient_cache_mutex);

		// Check it again since we only now have the mutex locked.
		if (s_transient_cache.size() < MAX_CACHED_TRANSIENTS)
		{
			// Clear this transient atomspace.
			atomspace->clear_transient();

			// Place this transient into the cache.
			s_transient_cache.push_back(atomspace);

			// The atomspace has been cached.
			atomspace_cached = true;
		}
	}

	// If we didn't cache the atomspace, then delete it.
	if (!atomspace_cached)
		delete atomspace;
}

/* ======================================================== */

TermMatchMixin::TermMatchMixin(AtomSpace* as) :
	_nameserver(nameserver())
{
	_temp_aspace = grab_transient_atomspace(as);
	_instor = new Instantiator(_temp_aspace);

	_connectives.insert(SEQUENTIAL_AND_LINK);
	_connectives.insert(SEQUENTIAL_OR_LINK);
	_connectives.insert(AND_LINK);
	_connectives.insert(OR_LINK);
	_connectives.insert(NOT_LINK);

	_as = as;
	_pat_bound_vars = nullptr;
	_gnd_bound_vars = nullptr;
}

TermMatchMixin::~TermMatchMixin()
{
	// If we have a transient atomspace, release it.
	if (_temp_aspace)
	{
		release_transient_atomspace(_temp_aspace);
		_temp_aspace = nullptr;
	}

	// Delete the instantiator.
	delete _instor;
}

void TermMatchMixin::set_pattern(const Variables& vars,
                                 const Pattern& pat)
{
	_vars = &vars;
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
bool TermMatchMixin::node_match(const Handle& npat_h,
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
bool TermMatchMixin::variable_match(const Handle& npat_h,
                                    const Handle& nsoln_h)
{
	// If the ungrounded term is not of type VariableNode, then just
	// accept the match. This allows any kind of node types to be
	// explicitly bound as variables.  However, the type VariableNode
	// gets special handling, below.
	Type pattype = npat_h->get_type();
	if (VARIABLE_NODE != pattype and GLOB_NODE != pattype) return true;

	// If the ungrounded term is a variable, then see if there
	// are any restrictions on the variable type.
	return _vars->is_type(npat_h, nsoln_h);
}

bool TermMatchMixin::scope_match(const Handle& npat_h,
                                 const Handle& nsoln_h)
{
	// If there are scoped vars, then accept anything that is
	// alpha-equivalent. (i.e. equivalent after alpha-conversion)
	if (_pat_bound_vars and _pat_bound_vars->is_in_varset(npat_h))
	{
		bool aok = _pat_bound_vars->is_alpha_convertible(npat_h,
		                  nsoln_h, *_gnd_bound_vars);
		return aok;
	}

	// Else, these need to be an exact match...
	return npat_h == nsoln_h;
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
bool TermMatchMixin::link_match(const PatternTermPtr& ptm,
                                const Handle& lsoln)
{
	const Handle& lpat = ptm->getHandle();

	// If the pattern is exactly the same link as the proposed
	// grounding, then its a perfect match.
	if (lpat == lsoln) return true;

	// Accept all ChoiceLink's by default! We will get another shot
	// at it when the contents of the ChoiceLink are examined.
	Type pattype = lpat->get_type();
	if (CHOICE_LINK == pattype) return true;

	// If types differ, no match
	Type soltype = lsoln->get_type();
	if (pattype != soltype) return false;

	// Reject mis-sized compares, unless the pattern has a glob in it.
	if (not ptm->hasGlobbyVar() and lpat->get_arity() != lsoln->get_arity())
		return false;

	// If the link is a ScopeLink, we need to deal with the
	// alpha-conversion of the bound variables in the scope link.
	if (_nameserver.isA(pattype, SCOPE_LINK))
	{
		// Unless it is quoted.
		if (ptm->isQuoted()) return true;

		// XXX The assert below -- if we hit this, then we have nested
		// scoped links. The correct fix would be to push these onto a
		// stack, and then alter scope_match() to walk the stack,
		// verifying alpha-convertability.
		OC_ASSERT(nullptr == _pat_bound_vars,
			"Not implemented! Need to implement a stack, here.");
		_pat_bound_vars = & ScopeLinkCast(lpat)->get_variables();
		_gnd_bound_vars = & ScopeLinkCast(lsoln)->get_variables();

		// This is interesting: the ground term need only satisfy
		// the pattern typing requirements.  We do not ask for equality:
		//     if (not _pat_bound_vars->is_equal(*_gnd_bound_vars))
		// because that prevents searches for narrowly-typed grounds
		// (as is done in the ForwardChainerUTest, see bug #934)
		// Alternately, a single variable can match an entire
		// VariableList (per bug #2070).
		if (*_pat_bound_vars != *_gnd_bound_vars
		      and not _pat_bound_vars->is_type(VARIABLE_LIST)
		      and not _pat_bound_vars->is_type(_gnd_bound_vars->varseq))
		{
			_pat_bound_vars = nullptr;
			_gnd_bound_vars = nullptr;
			return false;
		}
		return true;
	}

	// No reason to reject; proceed with the compare.
	return true;
}

bool TermMatchMixin::post_link_match(const Handle& lpat,
                                     const Handle& lgnd)
{
	Type pattype = lpat->get_type();
	if (_pat_bound_vars and _nameserver.isA(pattype, SCOPE_LINK))
	{
		_pat_bound_vars = nullptr;
		_gnd_bound_vars = nullptr;
	}

	// The StateLink has a single, unique closed-term value (or possibly
	// no value at all), and the check below discards all matches that
	// aren't closed form, i.e. all StateLinks with a variable as state.
	//
	// Note, BTW, that the value of the StateLink may have changed
	// between the time that the search was started, and the time that
	// we arrive here...
	if (STATE_LINK == pattype)
		return StateLinkCast(lgnd)->is_closed();

	return true;
}

void TermMatchMixin::post_link_mismatch(const Handle& lpat,
                                        const Handle& lgnd)
{
	Type pattype = lpat->get_type();
	if (_pat_bound_vars and _nameserver.isA(pattype, SCOPE_LINK))
	{
		_pat_bound_vars = nullptr;
		_gnd_bound_vars = nullptr;
	}
}

/// is_self_ground() -- Reject clauses that are grounded by themselves.
/// This code is needed in order to handle multiple complex, confusing
/// situations. The most serious of these is variables that are hidden
/// due to alpha-renaming -- they mostly look like ordinary variables
/// to the lower layers of the pattern matcher, but here, from the
/// top-level view of an entire clause, we can tell if they are alpha-
/// renamed (i.e. alpha-hidden) or not.  Additional complexities arise
/// due to a need to handle QuoteLinks, and to handle ChoiceLinks and
/// nested ChoiceLinks. So, sadly, this code is fairly complex. :-(
bool TermMatchMixin::is_self_ground(const Handle& ptrn,
                                    const Handle& grnd,
                                    const GroundingMap& term_gnds,
                                    const HandleSet& varset,
                                    Quotation quotation)
{
	Type ptype = ptrn->get_type();

	// Unwrap quotations, so that they can be compared properly.
	if (Quotation::is_quotation_type(ptype))
	{
		// Wow, if we are here, and patern==grnd, this must be
		// a self-grounding, as I don't believe there is any other
		// valid way to get to here.
		if (quotation.consumable(ptype) and ptrn == grnd) return true;

		const Handle& qpat = ptrn->getOutgoingAtom(0);

		quotation.update(ptype);
		return is_self_ground(qpat, grnd, term_gnds, varset, quotation);
	}

	// Only unquoted variables...
	if (quotation.is_unquoted() and
	    ((ptype == VARIABLE_NODE ) or (ptype == GLOB_NODE)))
	{
		return (varset.end() != varset.find(grnd));
	}

	// To pass an updated quotation to the recursive calls
	quotation.update(ptype);

	// Handle choice-links.
	if (CHOICE_LINK == ptype)
	{
		if (ptrn == grnd) return true;

		const HandleSeq& pset = ptrn->getOutgoingSet();
		for (const Handle& ch: pset)
		{
			const auto pr = term_gnds.find(ch);
			Type cht = ch->get_type();
			if (pr != term_gnds.end() or CHOICE_LINK == cht)
			{
				if (is_self_ground(ch, grnd, term_gnds, varset, quotation))
					return true;
			}
		}
		return false;
	}

	// Just assume matches were carried out correctly.
	// Do not try to get fancy, here.
	if (not ptrn->is_link()) return false;
	if (not grnd->is_link()) return false;

	// Recursive call.
	const HandleSeq& pset = ptrn->getOutgoingSet();
	const HandleSeq& gset = grnd->getOutgoingSet();
	size_t pari = pset.size();

	// punt on glob verification
	if (pari != gset.size()) return false;

	// Special handling for ScopeLinks. Any bound variables in the
	// ScopeLink that happen to have exactly the same name as a bound
	// variable in the pattern will hide/obscure the variable in the
	// pattern. Or rather: here is where we hide it.  Tedious.
	if (_nameserver.isA(grnd->get_type(), SCOPE_LINK))
	{
		// Step 1: Look to see if the scope link binds any of the
		// variables that the pattern also binds.
		const Variables& slv = ScopeLinkCast(grnd)->get_variables();
		HandleSet hidden = opencog::set_intersection(slv.varset, varset);

		// Step 2: If there are hidden variables, then remove them
		// before recursing donwards.
		if (0 < hidden.size())
		{
			// Make a copy with visible variables only
			HandleSet vcopy = opencog::set_difference(varset, hidden);

			// Recurse using only the visible variables.
			for (size_t i=0; i<pari; i++)
			{
				if (is_self_ground(pset[i], gset[i], term_gnds, vcopy, quotation))
					return true;
			}
			return false;
		}

		// Step 3: if we are here, there were no hidden vars.
		// So just fall through and do the normal recursion below.
	}

	for (size_t i=0; i<pari; i++)
	{
		if (is_self_ground(pset[i], gset[i], term_gnds, varset, quotation))
			return true;
	}

	return false;
}

/**
 * Called to accept or reject a top-level clause.
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
bool TermMatchMixin::clause_match(const Handle& ptrn,
                                  const Handle& grnd,
                                  const GroundingMap& term_gnds)
{
	// Is the pattern same as the ground?
	// if (ptrn == grnd) return false;
	// Well, in an ideal world, it intuitively makes sense to reject
	// clauses that are grounded by themselves. In the real world, this
	// runs afoul of several unusual situations. The one we care about
	// is an evaluatable clause which contains no variables.  In this
	// case, we need to accept the match in order for a SatisfactionLink
	// to get valued correctly. Tested in SequenceUTest.
	// if (ptrn == grnd) return false;

	// This if-statement handles the case given in the callback description.
	// It is tested by EvaluationUTest.
	if (ptrn->get_type() == VARIABLE_NODE and
	    grnd->get_type() == EVALUATION_LINK and
	    0 < grnd->get_arity() and
	    (grnd->getOutgoingAtom(0)->get_type() == GROUNDED_PREDICATE_NODE or
	    grnd->getOutgoingAtom(0)->get_type() == DEFINED_PREDICATE_NODE))
	{
		DO_LOG({LAZY_LOG_FINE << "Evaluate the grounding clause=" << std::endl
		              << grnd->to_short_string() << std::endl;})

		// We make two awkard asumptions here: the ground term itself
		// does not contain any variables, and so does not need any
		// further grounding. This actually seems reasonable. The second
		// assumption is that the EvaluationLink is actually evaluatable,
		// which seems reasonable, except that everything else in the
		// default callback ignores the TV on EvaluationLinks. So this
		// is kind-of schizophrenic here.  Not sure what else to do.
		_temp_aspace->clear();
		bool crispy = EvaluationLink::crisp_eval_scratch(_as, grnd, _temp_aspace);

		DO_LOG({LAZY_LOG_FINE << "Clause_match evaluation yielded: "
		                      << crispy << std::endl;})

		return crispy;
	}

	return not is_self_ground(ptrn, grnd, term_gnds, _vars->varset);
}

/**
 * The default semantics here is to reject a match if the option
 * clauses are detected.  This is in keeping with the semantics
 * AbsentLink: a match is possible only if the indicated clauses
 * are absent!
 *
 * To recap:
 *   If ground is found, return false;
 *   If no ground is found, return true.
 */
bool TermMatchMixin::optional_clause_match(const Handle& ptrn,
                                           const Handle& grnd,
                                           const GroundingMap& term_gnds)
{
	// If any grounding at all was found, reject it.
	if (grnd)
	{
		if (not is_self_ground(ptrn, grnd, term_gnds, _vars->varset))
			_optionals_present = true;
		return false;
	}

	// No grounding was found directly, but that may be because
	// this is a virtual-optional clause. That is, this clause
	// might be bridging across two disconnected components;
	// some of the component combinations may have a grounding,
	// while others do not.  So we have to check.
	Handle gopt;
	try
	{
		gopt = HandleCast(_instor->instantiate(ptrn, term_gnds, true));
	}
	catch (const SilentException& ex)
	{
		// The instantiation above can throw an exception if the
		// it is ill-formed. If so, assume the opt clause is absent.
		return true;
	}

	// If the result is a self-match, we don't mind.
	// Its only bad if something else got built.
	if (gopt == ptrn) return true;

	// If the grounding we just built can be found in the atomspace,
	// then we must reject it.
	if (_as->get_atom(gopt)) return false;

	return true;
}

/* ======================================================== */

/* This implements AlwaysLink (the non-scoped vesion of ForAllLink
 * used by the pattern matcher.) The AlwaysLink must always be
 * satsifed, every time it is called, from the begining of the
 * search to the end.  The AlwaysLinks is satsified whenever
 * ptrn==grnd, and otherwise, if fails. That is, if ptrn==nullptr
 * then there is some grounding of (all of) the other clauses of
 * the pattern, with AlwaysLink failing to be satisfied. Reject
 * this case, now and forever. (viz, this is stateful.)
 */
bool TermMatchMixin::always_clause_match(const Handle& ptrn,
                                         const Handle& grnd,
                                         const GroundingMap& term_gnds)
{
	return grnd != nullptr;
}

/* ======================================================== */

IncomingSet TermMatchMixin::get_incoming_set(const Handle& h, Type t)
{
	return h->getIncomingSetByType(t, _as);
}

/* ======================================================== */
// FIXME: the code below is festooned with various FIXME's, stating
// that, basically, the evaluation of terms in the presence of
// grounding atoms is a bit messed up, and not fully correctly, cleanly
// done. So this needs a clean re-implementation, with proper
// beta-reduction with the groundings, followed by proper evaluation.

bool TermMatchMixin::eval_term(const Handle& virt,
                                      const GroundingMap& gnds)
{
	// Evaluation of the link requires working with an atomspace
	// of some sort, so that the atoms can be communicated to scheme or
	// python for the actual evaluation. We don't want to put the
	// proposed grounding into the "real" atomspace, because the
	// grounding might be insane.  So we put it here. This is not
	// very efficient, but will do for now...

	ValuePtr vp;
	try
	{
		// XXX FIXME. This is kind-of/mostly wrong. What we *really*
		// want to do is to plug the grounds into the virt expression,
		// then evaluate the virt expression, and see if it is true.
		// So, Instantiator::instantiate() does this, but then it
		// does too much; it also executes. Which is more than what
		// is wanted.
		vp = _instor->instantiate(virt, gnds, true);
	}
	catch (const SilentException& ex)
	{
		// The evaluation above can throw an exception if the
		// instantiation turns out to be ill-formed. If so assume it
		// has failed.
		//
		// TODO: it would probably be preferable to put this try/catch
		// around the eval_sentence call in
		// Satisfier::search_finished, because in case such virtual
		// term is embdded in a NotLink returning false here is gonna
		// validate the NotLink, which might not be desirable.
		return false;
	}

	// Perhaps it already evaluated down to a truth-value.
	if (not vp->is_atom())
	{
		TruthValuePtr tvp(TruthValueCast(vp));
		if (nullptr == tvp)
			throw InvalidParamException(TRACE_INFO,
		            "Expecting a TruthValue for an evaluatable link: %s\n",
		            virt->to_short_string().c_str());

		return crisp_truth_from_tv(tvp);
	}

	// Its an atom... now we have to evaluate it.
	Handle gvirt(HandleCast(vp));

	DO_LOG({LAZY_LOG_FINE << "Enter eval_term CB with virt=" << std::endl
	              << virt->to_short_string() << std::endl;})
	DO_LOG({LAZY_LOG_FINE << "Grounded by gvirt=" << std::endl
	              << gvirt->to_short_string() << std::endl;})

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

	// The instantiator would have taken care of expanding out
	// and executing any FunctionLinks and the like.  Just use
	// the TV value on the resulting atom.
	//
	// However, we also want to have a side-effect: the result of
	// executing one of these things should be placed into the atomspace.
	Type vty = virt->get_type();
	if (EXECUTION_OUTPUT_LINK == vty or
	    DEFINED_SCHEMA_NODE == vty or
	    _nameserver.isA(vty, FUNCTION_LINK))
	{
		gvirt = _as->add_atom(gvirt);
		TruthValuePtr tvp = gvirt->getTruthValue();

		// Avoid null-pointer dereference if user specified a bogus evaluation.
		// i.e. an evaluation that failed to return a TV.
		if (nullptr == tvp)
			throw InvalidParamException(TRACE_INFO,
			        "Expecting a TruthValue for an evaluatable link: %s\n",
			         gvirt->to_short_string().c_str());

		DO_LOG({LAZY_LOG_FINE << "Eval_term evaluation yielded tv="
		                      << tvp->to_string() << std::endl;})

		return crisp_truth_from_tv(tvp);
	}

	_temp_aspace->clear();
	try
	{
		bool crispy = EvaluationLink::crisp_eval_scratch(_as, gvirt, _temp_aspace, true);
		DO_LOG({LAZY_LOG_FINE << "Eval_term evaluation yielded crisp-tv="
		                      << crispy << std::endl;})
		return crispy;
	}
	catch (const SilentException& ex)
	{
		// The do_evaluate()/do_eval_scratch() above can throw if
		// it is given ungrounded expressions. It can be given
		// ungrounded expressions if no grounding was found, and
		// a final pass, run by the search_finished() callback,
		// puts us here. So handle this case gracefully.
		return false;
	}
}

/* ======================================================== */

/**
 * This implements the evaluation of a classical binary-logic
 * "sentence": a well-formed formula with no free variables,
 * having a crisp true/false truth value.  Here, "top" holds
 * the sentence (with variables), 'gnds' holds the bindings of
 * variables to values.
 */
bool TermMatchMixin::eval_sentence(const Handle& top,
                                   const GroundingMap& gnds)
{
	DO_LOG({LAZY_LOG_FINE << "Enter eval_sentence CB with top=" << std::endl
	              << top->to_short_string() << std::endl
	              << "grounds=" << gnds << std::endl;})

	Type term_type = top->get_type();
	if (VARIABLE_NODE == term_type)
		return eval_term(top, gnds);

	if (not top->is_link())
		throw InvalidParamException(TRACE_INFO,
	            "Not expecting a Node, here %s\n",
	            top->to_short_string().c_str());

	const HandleSeq& oset = top->getOutgoingSet();

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
	         _nameserver.isA(term_type, VIRTUAL_LINK))
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
			if (gnds.end() == gnds.find(h)) return false;
		}
		return true;
	}
	else if (ABSENT_LINK == term_type)
	{
		// If *any* clause in the AbsentLink has been grounded, then
		// return false.  That is, AbsentLink behaves like an AndLink
		// for term-absence.  Note that this conflicts with static
		// analysis in PatternLink, which insists on an arity of one.
		// Viz "must all be absent"? or "if any are absent"?
		//
		// AbsentLink is same as NotLink PresentLink.
		for (const Handle& h : oset)
		{
			// If no grounding, that's good, try the next one.
			if (gnds.end() == gnds.find(h)) continue;

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
		//
		// XXX FIXME: worse: this cannot possibly be right when
		// the ChoiceLink contains presentLinks.
		for (const Handle& h : oset)
		{
			if (gnds.end() == gnds.find(h)) continue;
			return true;
		}
		return false;
	}

	// --------------------------------------------------------
	// If we are here, then what we have is some atom that is not
	// normally "truth-valued". We can do one of two things:
	//
	// a) Throw an exception and complain, and tell the user to say
	//  `(GreaterThan (TruthValueOf X) (Number 0.5))`, to get a yes/no
	//   decision.
	// b) Just assume the user wanted the above, by default.
	//
	// Currently, the below implements choice (b): i.e. it gets the TV
	// of this atom, and checks to see if it is greater than 0.5 or not.
	// In the long run, we might want to switch to option (a) ... ?
	//
	// There are several minor issues: 1) we need to check the TV
	// of the grounded atom, not the TV of the pattern, and 2) if
	// the atom is executable, we need to execute it.
	const auto& g = gnds.find(top);
	if (gnds.end() != g)
	{
		TruthValuePtr tvp(g->second->getTruthValue());
		DO_LOG({LAZY_LOG_FINE << "Non-logical atom has tv="
		              << tvp->to_string() << std::endl;})
		return crisp_truth_from_tv(tvp);
	}

	// If it's not grounded, then perhaps its executable.
	return eval_term(top, gnds);
}

/* ===================== END OF FILE ===================== */
