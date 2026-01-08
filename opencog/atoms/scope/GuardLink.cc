/*
 * GuardLink.cc
 *
 * Copyright (C) 2026 BrainyBlaze Dyamics LLC
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
 */

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/flow/LinkSignatureLink.h>
#include <opencog/atoms/free/FindUtils.h>
#include <opencog/atoms/pattern/PatternUtils.h>
#include <opencog/atoms/signature/TypeNode.h>
#include <opencog/atoms/signature/TypeUtils.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/oc_assert.h>

#include "GuardLink.h"
#include "GlobMatch.h"

using namespace opencog;

GuardLink::GuardLink(const HandleSeq&& oset, Type t)
	: ScopeLink(std::move(oset), t), _recursive_glob(false)
{
	if (not nameserver().isA(t, GUARD_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw SyntaxException(TRACE_INFO,
			"Expecting a GuardLink, got %s", tname.c_str());
	}
}

// ===============================================================

void GuardLink::init_globby_terms(void) const
{
	// Locate all GlobNodes in the pattern body
	FindAtoms fgn(GLOB_NODE, true);
	fgn.search_set(_body);
	for (const Handle& sh : fgn.least_holders)
		_globby_terms.insert(sh);
}

void GuardLink::do_init(void) const
{
	init_globby_terms();

	// Unwrap PresentLink
	// XXX TODO We will need to handle AbsentLink and other stuffs.
	auto unwrap_present = [](const Handle& h) -> Handle {
		if (PRESENT_LINK == h->get_type() and 1 == h->get_arity())
			return h->getOutgoingAtom(0);
		return h;
	};

	// AndLink is treated not as a literal, but as a wrapper
	// of all required pattern-matching clasues. Just like
	// how PatternLink does it.
	// The `can_evaluate()` is taken from PatternUtils, and
	// provides consistent behavior.
	// XXX TODO Need to deal with ChoiceLink and other stuffs.
	if (AND_LINK == _body->get_type())
	{
		for (const Handle& clause : _body->getOutgoingSet())
		{
			if (can_evaluate(clause))
				_guard_clauses.push_back(clause);
			else
			{
				if (_match_pattern)
					throw SyntaxException(TRACE_INFO,
						"GuardLink body can have only one pattern term, got multiple");
				_match_pattern = unwrap_present(clause);
			}
		}
		return;
	}

	// Single clause body
	if (can_evaluate(_body))
		_guard_clauses.push_back(_body);
	else
		_match_pattern = unwrap_present(_body);
}

void GuardLink::init(void) const
{
	// Lazy, thread-safe initialization
	std::call_once(_init_flag, [this]() { this->do_init(); });
}

// ===============================================================

bool GuardLink::eval_guard(const ValueMap& valmap,
                           AtomSpace* scratch, bool silent) const
{
	// Evaluate each guard clause
	for (const Handle& clause : _guard_clauses)
	{
		Handle grounded = _variables.substitute_nocheck(clause, valmap);
		if (not grounded->bevaluate(scratch, silent))
			return false;
	}
	return true;
}

bool GuardLink::guard(const ValuePtr& gnd, ValueMap& valmap,
                      AtomSpace* scratch, bool silent) const
{
	init();

	// If there are silent exceptions, then clearly the guard fails.
	try
	{
		// Extract groundings from the pattern term
		if (_match_pattern)
		{
			bool ok = extract(_match_pattern, gnd, valmap, scratch, true);
			if (not ok) return false;
		}
		if (_guard_clauses.empty()) return true;
		return eval_guard(valmap, scratch, true);
	}
	catch (const SilentException&)
	{
		return false;
	}
}

// ===============================================================

/// Recursive tree-compare-and-extract grounding values.
///
/// Compare the pattern tree `termpat` with the grounding tree `ground`.
/// If a variable in `termpat` corresponds with a variable in `ground`,
/// then add that correspondence pair to `valmap`. Type-checking is
/// performed during the match-up, so if the variable type does not
/// match the ground type, false is returned.  False is also returned
/// if the trees miscompare in other ways (mismatched link arity,
/// mis-matched atom type, two conflicting groundings for the same
/// variable).
///
/// Any executable terms in `ground` are executed prior to comparison.
///
/// If false is returned, the contents of valmap are invalid. If true
/// is returned, valmap contains the extracted values.
///
bool GuardLink::extract(const Handle& termpat,
                        const ValuePtr& gnd,
                        ValueMap& valmap,
                        AtomSpace* scratch, bool silent,
                        const Quotation& quotation) const
{
	Type t = termpat->get_type();

	// If its a variable, then see if we know its value already;
	// Do this first, before attempting execution; this avoids
	// some recursive uglies.
	if (VARIABLE_NODE == t and 0 < _variables.varset.count(termpat))
	{
		// If we already have a value, the value must be identical.
		auto val = valmap.find(termpat);
		if (valmap.end() != val)
			return (val->second == gnd);

		// If the type is acceptable; don't go any further.
		if (_variables.is_type(termpat, gnd))
		{
			valmap.emplace(std::make_pair(termpat, gnd));
			return true;
		}
	}

	// Let the conventional type-checker deal with complicated types.
	// LinkSignatureLinks might contain vars; deal with these below.
	if (termpat->is_type(TYPE_NODE) or
	    (termpat->is_type(TYPE_OUTPUT_SIG) and
	       (not termpat->is_type(LINK_SIGNATURE_LINK))))
		return value_is_type(termpat, gnd);

	// Execute the proposed grounding term.
	ValuePtr vgnd(gnd);
	Handle hgnd(HandleCast(gnd));
	if (hgnd and hgnd->is_executable())
	{
		vgnd = hgnd->execute(scratch, silent);
		if (nullptr == vgnd) return false;
	}

	// Consume quotation
	if (quotation.consumable(t))
	{
		Quotation uquot(quotation);
		uquot.update(t);
		return extract(termpat->getOutgoingAtom(0), vgnd, valmap,
		               scratch, silent, uquot);
	}

	if (VARIABLE_NODE == t and 0 < _variables.varset.count(termpat))
	{
		// If we already have a value, the value must be identical.
		auto val = valmap.find(termpat);
		if (valmap.end() != val)
			return (val->second == vgnd);

		// Check the type of the value.
		if (not _variables.is_type(termpat, vgnd)) return false;

		// If we are here, everything looks good. Record and return.
		valmap.emplace(std::make_pair(termpat, vgnd));
		return true;
	}

	if (GLOB_NODE == t and 0 < _variables.varset.count(termpat))
	{
		// Check the type of the value.
		if (not _variables.is_type(termpat, vgnd)) return false;

		// If we are deep in doing glob compares, then all that was
		// needed is the result of the type compare.
		if (_recursive_glob) return true;

		// Globs are always wrapped by a List or LinkValue.
		if (vgnd->is_atom())
			valmap.emplace(std::make_pair(termpat,
			               createLink(LIST_LINK, HandleCast(vgnd))));
		else
			valmap.emplace(std::make_pair(termpat, createLinkValue(vgnd)));
		return true;
	}

	// Special-case for ChoiceLinks in the body of the pattern.
	if (CHOICE_LINK == t)
	{
		for (const Handle& choice : termpat->getOutgoingSet())
		{
			ValueMap vcopy(valmap);
			if (extract(choice, vgnd, vcopy, scratch, silent, quotation))
			{
				valmap.swap(vcopy);
				return true;
			}
		}
		return false;
	}

	// If they are (non-variable) nodes, they must be identical.
	if (not termpat->is_link())
		return (termpat == vgnd);

	// Pattern is a LinkSig. LinkSigs have a typespec.
	if (LINK_SIGNATURE_LINK == t)
	{
		Handle ts = LinkSignatureLinkCast(termpat)->get_typespec();
		if (not TypeNodeCast(ts)->is_kind(vgnd->get_type()))
			return false;

		ValuePtr patval = termpat->execute();
		if ((not vgnd->is_type(LINK_VALUE)) and
		    (not vgnd->is_type(LINK)))
			return (*patval == *vgnd);
	}
	// Else straight-up see if pattern and grounding types agree.
	else if (t != vgnd->get_type()) return false;

	// From here on out, we prepare to compare Links.
	const HandleSeq& tlo = termpat->getOutgoingSet();
	size_t tsz = tlo.size();
	size_t off = 0;
	if (LINK_SIGNATURE_LINK == t)
		{ tsz--; off = 1; }

	// If no glob nodes, just compare links side-by-side.
	if (0 == _globby_terms.count(termpat))
	{
		if (vgnd->is_atom())
		{
			const HandleSeq& glo = HandleCast(vgnd)->getOutgoingSet();
			size_t gsz = glo.size();
			if (tsz != gsz) return false;
			for (size_t i=0; i<tsz; i++)
			{
				if (not extract(tlo[i+off], glo[i], valmap, scratch, silent, quotation))
					return false;
			}
			return true;
		}

		if (vgnd->is_type(LINK_VALUE))
		{
			const auto& glo = LinkValueCast(vgnd)->value();
			size_t gsz = glo.size();
			if (tsz != gsz) return false;
			for (size_t i=0; i<tsz; i++)
			{
				if (not extract(tlo[i+off], glo[i], valmap, scratch, silent, quotation))
					return false;
			}
			return true;
		}

		// Solitary Value case
		if ((LINK_SIGNATURE_LINK != t) or (1 != tsz))
			return false;

		valmap.emplace(std::make_pair(tlo[1], vgnd));
		return true;
	}

	// If we are here, then there's a glob to be matched.
	_recursive_glob = true;

	// Helper lambda to perform glob matching
	auto do_glob_match = [&]<typename GroundSeq>(const GroundSeq& glo) -> bool
	{
		GlobValidateCallback<GroundSeq> validate =
			[&](const Handle& pattern_elem,
			    const typename GroundSeq::value_type& ground_elem,
			    ValueMap& bindings) -> bool {
				return this->extract(pattern_elem, ground_elem, bindings,
				                     scratch, silent, quotation);
			};

		GlobMakeValueCallback<GroundSeq> make_value;
		if constexpr (std::is_same_v<GroundSeq, HandleSeq>)
			make_value = [](const HandleSeq& matched_seq) -> ValuePtr {
				return createLink(HandleSeq(matched_seq), LIST_LINK);
			};
		else
			make_value = [](const ValueSeq& matched_seq) -> ValuePtr {
				return createLinkValue(ValueSeq(matched_seq));
			};

		return glob_match(tlo, glo, valmap, &_variables, validate, make_value, off, tsz);
	};

	// Handle different ground value types
	if (vgnd->is_link())
		return do_glob_match(HandleCast(vgnd)->getOutgoingSet());

	if (vgnd->is_type(LINK_VALUE))
		return do_glob_match(LinkValueCast(vgnd)->value());

	// Single value case
	return do_glob_match(ValueSeq({vgnd}));
}

DEFINE_LINK_FACTORY(GuardLink, GUARD_LINK)

/* ===================== END OF FILE ===================== */
