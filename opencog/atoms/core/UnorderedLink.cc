/*
 * UnorderedLink.cc
 *
 * Copyright (C) 2017 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009, 2015, 2017
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

#include "UnorderedLink.h"

using namespace opencog;

UnorderedLink::UnorderedLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (not nameserver().isA(t, UNORDERED_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an UnorderedLink, got %s", tname.c_str());
	}

	// Place into arbitrary, but deterministic order. We use
	// content (hash) based less, to avoid variations due to
	// address-space randomization.
	std::sort(_outgoing.begin(), _outgoing.end(),
		content_based_handle_less());
}

UnorderedLink::UnorderedLink(const HandleSet& oset, Type t)
	: Link(HandleSeq(), t)
{
	if (not nameserver().isA(t, UNORDERED_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an UnorderedLink, got %s", tname.c_str());
	}

	// We need a vector not a set.
	for (const Handle& h: oset)
		_outgoing.push_back(h);

	// Place into arbitrary, but deterministic order.
	// Actually, this should already be in sorted order, because
	// HandleSet is already sorted by content_based_handle_less().
	// But it can't hurt to do it again, to avoid insanity.
	std::sort(_outgoing.begin(), _outgoing.end(),
		content_based_handle_less());
}

// ---------------------------------------------------------------

DEFINE_LINK_FACTORY(UnorderedLink, UNORDERED_LINK)

/* ===================== END OF FILE ===================== */
