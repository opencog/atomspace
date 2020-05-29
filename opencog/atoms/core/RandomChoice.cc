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
ValuePtr RandomChoiceLink::execute(AtomSpace* as, bool silent)
{
	size_t ary = _outgoing.size();
	if (0 == ary) return ValuePtr();

	Handle ofirst(_outgoing[0]);

	// We need to have our first arg to be a set or a list or
	// something of that sort.
	if (ofirst->is_executable())
		ofirst = HandleCast(ofirst->execute(as, silent));

	// Special-case handling for SetLinks, so it works with
	// dynamically-evaluated PutLinks ...
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
