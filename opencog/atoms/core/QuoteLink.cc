/*
 * QuoteLink.cc
 *
 * Copyright (C) 2025 OpenCog Foundation
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

#include "QuoteLink.h"

using namespace opencog;

QuoteLink::QuoteLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	// Type must be as expected
	if (not nameserver().isA(t, QUOTE_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a QuoteLink, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// Execute the QuoteLink by returning its wrapped content.
/// If the content is an UnquoteLink, apply the involution rule:
/// (Quote (Unquote X)) -> X
ValuePtr QuoteLink::execute(AtomSpace* as, bool silent)
{
	// Handle empty QuoteLink
	if (0 == _outgoing.size())
		return Handle::UNDEFINED;

	// Single argument case
	if (1 == _outgoing.size())
	{
		Handle content = _outgoing[0];

		// Check for the involution pattern: (Quote (Unquote X)) -> X
		if (content->get_type() == UNQUOTE_LINK)
		{
			const HandleSeq& unquote_oset = content->getOutgoingSet();
			if (1 == unquote_oset.size())
				return unquote_oset[0];
		}

		return content;
	}

	// Multiple arguments case: process each one
	HandleSeq results;
	for (const Handle& h : _outgoing)
	{
		// Check for involution on each argument
		if (h->get_type() == UNQUOTE_LINK)
		{
			const HandleSeq& unquote_oset = h->getOutgoingSet();
			if (1 == unquote_oset.size())
				results.push_back(unquote_oset[0]);
			else
				results.push_back(h);
		}
		else
		{
			results.push_back(h);
		}
	}

	// Return as a Link to preserve structure
	return createLink(std::move(results), LIST_LINK);
}

DEFINE_LINK_FACTORY(QuoteLink, QUOTE_LINK)

/* ===================== END OF FILE ===================== */
