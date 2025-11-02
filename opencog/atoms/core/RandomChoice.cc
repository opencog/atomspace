/*
 * RandomChoice.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#include <opencog/util/mt19937ar.h>

#include <opencog/atoms/value/LinkValue.h>
#include "FunctionLink.h"
#include "NumberNode.h"
#include "RandomChoice.h"

using namespace opencog;

static MT19937RandGen randy(43);

RandomChoiceLink::RandomChoiceLink(const HandleSeq&& oset, Type t)
	: FunctionLink(std::move(oset), t)
{
	if (not nameserver().isA(t, RANDOM_CHOICE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an RandomChoiceLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// When executed, this will randomly select and return an atom
/// in it's outgoing set. The selection can use either a uniform or
/// a weighted distribution.  Two different formats are used to specify
/// weights; if neither of these are used, a uniform distribution is
/// used.
///
/// One way to specify weights is to use a weight-vector:
///
///    RandomChoiceLink
///        ListLink
///           NumberNode
///           ...
///           NumberNode
///        ListLink
///           AtomA
///           ...
///           AtomZ
///
/// With the above format, the atoms A..Z will be selected with
/// distribution weights taken from the NumberNodes. The probability of
/// selection is in *proportion* to the weights; viz the probability is
/// given by dividing a given weight by the sum of the weights.
/// The Number of AtomsA..Z MUST match the number of NumberNodes!
///
/// A second way to specify weights is much more GetLink friendly:
///
///    RandomChoiceLink
///        SetLink
///           ListLink
///              NumberNode1
///              AtomA
///           ListLink
///              NumberNode2
///              AtomB
///              ...
///           ListLink
///              NumberNodeN
///              AtomZ
///
/// Here, the weights and atoms are paired. The pairs appear in a
/// SetLink, which is an unordered link, and is the link type returned
/// by the GetLink query function.
///
/// If neither of the above two formats appear to hold, then it is
/// assumed that the RandomChoiceLink simply holds a list of atoms;
/// these are selected with uniform weighting.  Viz:
///
///    RandomChoiceLink
///        AtomA
///        AtomB
///        ...
///        AtomZ
///
/// or the GetLink-friendly format:
///
///    RandomChoiceLink
///        SetLink
///           AtomA
///           AtomB
///           ...
///           AtomZ
///

// XXX FIXME - fix this so it can also choose a single value
// out of a vector of values.
//
// XXX ULTRA FIXME the unwrapping and jiggering of LIST_LINK
// LINK_VALUE and SET_LINK is ugly and duplicated code and needs
// some clean rewrite to make it less nasty. I've done it fairly
// cleanly in other place, like in the reduct directory, with 
// numeric values. Need to redo that here.
ValuePtr RandomChoiceLink::execute(AtomSpace* as, bool silent)
{
	size_t ary = _outgoing.size();
	if (0 == ary) return ValuePtr();

	// We need to have our first arg to be a set or a list or
	// something of that sort.
	ValuePtr vfirst = _outgoing[0];
	if (_outgoing[0]->is_executable())
		vfirst = _outgoing[0]->execute(as, silent);

	// Handle LinkValue case (e.g., from MeetLink)
	if (vfirst->is_type(LINK_VALUE))
	{
		LinkValuePtr lv = LinkValueCast(vfirst);
		if (1 == ary)
		{
			// Search for ListLink pairs, w/car of pair a number.
			HandleSeq choices;
			std::vector<double> weights;
			for (const ValuePtr& v : lv->value())
			{
				// Handle both ListLink (Atom) and LinkValue cases
				if (v->is_type(LINK_VALUE))
				{
					// LinkValue case: each element is LinkValue(weight, choice)
					LinkValuePtr lv_pair = LinkValueCast(v);
					if (2 != lv_pair->value().size()) goto uniform_lv;

					// Extract weight (should be a NumberNode Handle)
					Handle hw = HandleCast(lv_pair->value()[0]);
					if (hw && hw->is_executable())
						hw = HandleCast(hw->execute(as, silent));

					NumberNodePtr nn(NumberNodeCast(hw));
					if (nullptr == nn)
						throw SyntaxException(TRACE_INFO,
						       "Expecting a NumberNode");
					weights.push_back(nn->get_value());

					// Extract choice (second element)
					choices.push_back(HandleCast(lv_pair->value()[1]));
				}
				else
				{
					// ListLink (Atom) case
					Handle h = HandleCast(v);
					if (nullptr == h or LIST_LINK != h->get_type()) goto uniform_lv;

					const HandleSeq& oset = h->getOutgoingSet();
					if (2 != oset.size()) goto uniform_lv;

					Handle hw(oset[0]);
					if (hw->is_executable())
						hw = HandleCast(hw->execute(as, silent));

					// XXX TODO if execute() above returns FloatValue, use that!
					NumberNodePtr nn(NumberNodeCast(hw));
					if (nullptr == nn) // goto uniform_lv;
						throw SyntaxException(TRACE_INFO,
						       "Expecting a NumberNode");
					weights.push_back(nn->get_value());
					choices.push_back(oset[1]);
				}
			}

			if (0 == weights.size())
				throw RuntimeException(TRACE_INFO,
					"Asked to choose element from empty set!");
			return choices[randy.rand_discrete(weights)];

uniform_lv:
			size_t lv_ary = lv->value().size();
			if (0 == lv_ary)
				throw RuntimeException(TRACE_INFO,
					"Asked to choose element from empty set!");
			return lv->value()[randy.randint(lv_ary)];
		}
	}

	if (not vfirst->is_atom())
		throw RuntimeException(TRACE_INFO,
			"Expecting SetLink or ListLink, got %s\n",
			vfirst->to_string().c_str());

	// Special-case handling for SetLinks, so it works with
	// dynamically-evaluated PutLinks ...
	Handle ofirst(HandleCast(vfirst));
	Type ot = ofirst->get_type();
	if (1 == ary and (SET_LINK == ot or LIST_LINK == ot))
	{
		// Search for ListLink pairs, w/car of pair a number.
		HandleSeq choices;
		std::vector<double> weights;
		for (const Handle& h : ofirst->getOutgoingSet())
		{
			if (LIST_LINK != h->get_type()) goto uniform;

			const HandleSeq& oset = h->getOutgoingSet();
			if (2 != oset.size()) goto uniform;

			Handle hw(oset[0]);
			if (hw->is_executable())
				hw = HandleCast(hw->execute(as, silent));

			// XXX TODO if execute() above returns FloatValue, use that!
			NumberNodePtr nn(NumberNodeCast(hw));
			if (nullptr == nn) // goto uniform;
				throw SyntaxException(TRACE_INFO,
				       "Expecting a NumberNode");
			weights.push_back(nn->get_value());
			choices.push_back(oset[1]);
		}

		if (0 == weights.size())
			throw RuntimeException(TRACE_INFO,
				"Asked to choose element from empty set!");
		return choices[randy.rand_discrete(weights)];

uniform:
		ary = ofirst->get_arity();
		if (0 == ary)
			throw RuntimeException(TRACE_INFO,
				"Asked to choose element from empty set!");
		return ofirst->getOutgoingAtom(randy.randint(ary));
	}

	// Weighted choices cannot be sets, since sets are unordered.
	if (2 == ary and LIST_LINK == ot)
	{
		const Handle& choices(_outgoing[1]);

		// ofirst are the weights
		if (ofirst->get_arity() != choices->get_arity())
			throw SyntaxException(TRACE_INFO,
				"Weights and choices must be the same size");

		// Weights need to be numbers, or must evaluate to numbers.
		std::vector<double> weights;
		for (Handle h : ofirst->getOutgoingSet())
		{
			// XXX FIXME, also allow a FloatValue!!
			if (h->is_executable())
				h = HandleCast(h->execute(as, silent));

			NumberNodePtr nn(NumberNodeCast(h));
			if (nullptr == nn)
				throw SyntaxException(TRACE_INFO,
				       "Expecting a NumberNode");
			weights.push_back(nn->get_value());
		}

		if (0 == weights.size())
			throw RuntimeException(TRACE_INFO,
				"Asked to choose element from empty set!");
		return choices->getOutgoingAtom(randy.rand_discrete(weights));
	}

	if (0 == ary)
		throw RuntimeException(TRACE_INFO,
			"Asked to choose element from empty set!");
	return _outgoing.at(randy.randint(ary));
}

DEFINE_LINK_FACTORY(RandomChoiceLink, RANDOM_CHOICE_LINK)

/* ===================== END OF FILE ===================== */
