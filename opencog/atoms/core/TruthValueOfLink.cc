/*
 * TruthValueOfLink.cc
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
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

#include <opencog/atomspace/AtomSpace.h>
#include "TruthValueOfLink.h"

using namespace opencog;

TruthValueOfLink::TruthValueOfLink(const HandleSeq& oset, Type t)
	: ValueOfLink(oset, t)
{
	if (not nameserver().isA(t, TRUTH_VALUE_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an TruthValueOfLink, got %s", tname.c_str());
	}
}

TruthValueOfLink::TruthValueOfLink(const Link &l)
	: ValueOfLink(l)
{
	// Type must be as expected
	Type tscope = l.get_type();
	if (not nameserver().isA(tscope, TRUTH_VALUE_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an TruthValueOfLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// When executed, this will return the TruthValue
ValuePtr TruthValueOfLink::execute(AtomSpace* as, bool silent)
{
	size_t ary = _outgoing.size();
	if (1 != ary)
		throw SyntaxException(TRACE_INFO, "Expecting one atom!");

	// We cannot know the TruthValue of the Atom unless we are
	// working with the unique version that sits in the AtomSpace!
	Handle ah(as->get_atom(_outgoing[0]));
	if (ah)
		return ValueCast(ah->getTruthValue());

	if (silent)
		throw SilentException();

	// If the user asked for a TV not in any atomspace,
	// what should we do? I dunno, so I'm throwing an error.
	throw InvalidParamException(TRACE_INFO,
		"Asked for TruthValue of atom not in any atomspace: %s",
		this->to_string().c_str());

	return Handle();
}

// =============================================================

StrengthOfLink::StrengthOfLink(const HandleSeq& oset, Type t)
	: ValueOfLink(oset, t)
{
	if (not nameserver().isA(t, STRENGTH_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an StrengthOfLink, got %s", tname.c_str());
	}
}

StrengthOfLink::StrengthOfLink(const Link &l)
	: ValueOfLink(l)
{
	// Type must be as expected
	Type tscope = l.get_type();
	if (not nameserver().isA(tscope, STRENGTH_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an StrengthOfLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// When executed, this will return the Strengths of all of the
/// atoms in the outgoing set.
ValuePtr StrengthOfLink::execute(AtomSpace* as, bool silent)
{
	std::vector<double> strengths;

	for (const Handle& h : _outgoing)
	{
		// Cannot take the strength of an ungrounded variable.
		Type t = h->get_type();
		if (VARIABLE_NODE == t or GLOB_NODE == t)
			return get_handle();

		// We cannot know the TruthValue of the Atom unless we are
		// working with the unique version that sits in the AtomSpace!
		Handle ah(as->get_atom(h));
		if (ah)
			strengths.push_back(ah->getTruthValue()->get_mean());
		else
		{
			if (silent)
				throw SilentException();

			// If the user asked for a TV not in any atomspace,
			// what should we do? I dunno, so I'm throwing an error.
			throw InvalidParamException(TRACE_INFO,
				"Asked for Strength of atom not in any atomspace: %s",
				this->to_string().c_str());
		}
	}

	return createFloatValue(strengths);
}

// =============================================================

ConfidenceOfLink::ConfidenceOfLink(const HandleSeq& oset, Type t)
	: ValueOfLink(oset, t)
{
	if (not nameserver().isA(t, CONFIDENCE_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an ConfidenceOfLink, got %s", tname.c_str());
	}
}

ConfidenceOfLink::ConfidenceOfLink(const Link &l)
	: ValueOfLink(l)
{
	// Type must be as expected
	Type tscope = l.get_type();
	if (not nameserver().isA(tscope, CONFIDENCE_OF_LINK))
	{
		const std::string& tname = nameserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an ConfidenceOfLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// When executed, this will return the Confidences of all of the
/// atoms in the outgoing set.
ValuePtr ConfidenceOfLink::execute(AtomSpace* as, bool silent)
{
	std::vector<double> confids;

	for (const Handle& h : _outgoing)
	{
		// Cannot take the confidence of an ungrounded variable.
		Type t = h->get_type();
		if (VARIABLE_NODE == t or GLOB_NODE == t)
			return get_handle();

		// We cannot know the TruthValue of the Atom unless we are
		// working with the unique version that sits in the AtomSpace!
		Handle ah(as->get_atom(h));
		if (ah)
			confids.push_back(ah->getTruthValue()->get_confidence());
		else
		{
			if (silent)
				throw SilentException();

			// If the user asked for a TV not in any atomspace,
			// what should we do? I dunno, so I'm throwing an error.
			throw InvalidParamException(TRACE_INFO,
				"Asked for Confidence of atom not in any atomspace: %s",
				this->to_string().c_str());
		}
	}

	return createFloatValue(confids);
}

DEFINE_LINK_FACTORY(TruthValueOfLink, TRUTH_VALUE_OF_LINK)
DEFINE_LINK_FACTORY(StrengthOfLink, STRENGTH_OF_LINK)
DEFINE_LINK_FACTORY(ConfidenceOfLink, CONFIDENCE_OF_LINK)

/* ===================== END OF FILE ===================== */
