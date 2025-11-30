/*
 * opencog/atoms/execution/EvaluatableLink.cc
 *
 * Copyright (C) 2009, 2013, 2014, 2015 Linas Vepstas
 * Copyright (C) 2025 BrainyBlaze Dynamics, LLC
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/free/FindUtils.h>
#include <opencog/atoms/grant/DefineLink.h>
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/reduct/NumericFunctionLink.h>
#include <opencog/atoms/scope/LambdaLink.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atomspace/AtomSpace.h>

#include "EvaluatableLink.h"
#include "EvaluationLink.h"

#include <algorithm>
#include <cmath>

using namespace opencog;

EvaluatableLink::EvaluatableLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t), _unordered(false)
{
	if (not nameserver().isA(t, EVALUATABLE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an EvaluatableLink, got %s", tname.c_str());
	}

	// A kind of a hack, to do what the UnorderedLink ctor does.
	// We do this mostly because we're lazy about writing lots
	// lots of c++ classes to handle each of the case statemens
	// below. Even though doing that would be "technically more
	// correct". But for now, this technical hack seems ... tolerable.
	// I really don't want to be pedantic for this stuff. It seems
	// OK to wing it, sometimes...
	if (nameserver().isA(t, UNORDERED_SIG))
	{
		_unordered = true;
		// Place into arbitrary, but deterministic order. We use
		// content (hash) based less, to avoid variations due to
		// address-space randomization.
		std::sort(_outgoing.begin(), _outgoing.end(),
			content_based_handle_less());
	}
}

// ---------------------------------------------------------------
// Helper functions moved from EvaluationLink.cc

/// Extract a single floating-point double out of an atom, that,
/// when executed, should yield a value containing a number.
/// Viz, either a NumberNode, or a FloatValue.
static double get_numeric_value(AtomSpace* as, bool silent, Handle h)
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

	if (silent)
		throw NotEvaluatableException();
	throw SyntaxException(TRACE_INFO,
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
/// a key somewhere.
static bool is_key(const Handle& h)
{
	for (const Handle& ho : h->getOutgoingSet())
		if (ho->isKey()) return true;

	return false;
}

/// Return true, if any Atom in the outgoing set is being used as
/// a message somewhere.
static bool is_message(const Handle& h)
{
	for (const Handle& ho : h->getOutgoingSet())
		if (ho->isMessage()) return true;

	return false;
}

/// Check for syntactic equality. Specifically, when comparing
/// atoms, the handles MUST be the same handle.
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

/// Check for semantic equality.
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

/// Check for alpha equivalence.
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

// ---------------------------------------------------------------

/// EvaluatableLink::bevaluate() -- evaluate this link, returning
/// a crisp-logic true/false value.
///
/// This handles evaluation of crisp Boolean-logic constructs including:
/// - Logical constants (TrueLink, FalseLink)
/// - Logical connectives (NotLink, AndLink, OrLink, SequentialAndLink, etc.)
/// - Comparison operations (GreaterThanLink, EqualLink, IdenticalLink, etc.)
///
bool EvaluatableLink::bevaluate(AtomSpace* scratch, bool silent)
{
	Type t = get_type();
	Handle evelnk(get_handle());

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
		for (const Handle& term : _outgoing)
			exec_or_eval(_atom_space, term, scratch, silent);

		if (TRUE_LINK == t) return true;
		return false;
	}

	// -------------------------
	// Crisp-binary-valued Boolean Logical connectives
	if (NOT_LINK == t)
	{
		return not EvaluationLink::crisp_eval_scratch(_atom_space,
		      _outgoing[0], scratch, silent);
	}
	if (AND_LINK == t)
	{
		for (const Handle& h : _outgoing)
		{
			bool tv = EvaluationLink::crisp_eval_scratch(_atom_space, h, scratch, silent);
			if (not tv) return false;
		}
		return true;
	}
	if (OR_LINK == t)
	{
		for (const Handle& h : _outgoing)
		{
			bool tv = EvaluationLink::crisp_eval_scratch(_atom_space, h, scratch, silent);
			if (tv) return true;
		}
		return false;
	}
	if (SEQUENTIAL_AND_LINK == t)
	{
		size_t arity = _outgoing.size();
		if (0 == arity) return true;

		// Is this tail-recursive? If so, then handle it.
		bool is_trec = is_tail_rec(evelnk, _outgoing[arity-1]);
		if (is_trec) arity--;

		// Loop at least once. If tail-recursive, loop forever.
		do
		{
			for (size_t i=0; i<arity; i++)
			{
				bool tv = EvaluationLink::crisp_eval_scratch(_atom_space, _outgoing[i], scratch, silent);
				if (not tv) return false;
			}
		} while (is_trec);
		return true;
	}
	if (SEQUENTIAL_OR_LINK == t)
	{
		size_t arity = _outgoing.size();
		if (0 == arity) return false;

		// Is this tail-recursive? If so, then handle it.
		bool is_trec = is_tail_rec(evelnk, _outgoing[arity-1]);
		if (is_trec) arity--;

		// Loop at least once. If tail-recurive, loop forever.
		do
		{
			for (size_t i=0; i<arity; i++)
			{
				bool tv = EvaluationLink::crisp_eval_scratch(_atom_space, _outgoing[i], scratch, silent);
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

	throw RuntimeException(TRACE_INFO,
		"Implementation Error: Cannot evaluate %s", to_string().c_str());
}

// ---------------------------------------------------------------

DEFINE_LINK_FACTORY(EvaluatableLink, EVALUATABLE_LINK)

/* ===================== END OF FILE ===================== */
