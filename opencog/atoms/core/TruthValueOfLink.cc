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
ProtoAtomPtr TruthValueOfLink::execute() const
{
	size_t ary = _outgoing.size();
	if (1 != ary)
		throw SyntaxException(TRACE_INFO, "Expecting one atom!");

	return ProtoAtomCast(_outgoing[0]->getTruthValue());
}

DEFINE_LINK_FACTORY(TruthValueOfLink, TRUTH_VALUE_OF_LINK)

/* ===================== END OF FILE ===================== */
