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
#include <opencog/atomspace/AtomSpace.h>

#include "UnorderedLink.h"

using namespace opencog;

UnorderedLink::UnorderedLink(const HandleSeq& oset, Type t)
	: Link(oset, t)
{
	if (not classserver().isA(t, UNORDERED_LINK))
	{
		const std::string& tname = classserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an UnorderedLink, got %s", tname.c_str());
	}

	// Place into arbitrary, but deterministic order.
	std::sort(_outgoing.begin(), _outgoing.end(), handle_less());
}

UnorderedLink::UnorderedLink(const Link& l)
	: Link(l)
{
	// Type must be as expected
	Type tscope = l.get_type();
	if (not classserver().isA(tscope, UNORDERED_LINK))
	{
		const std::string& tname = classserver().getTypeName(tscope);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an UnorderedLink, got %s", tname.c_str());
	}

	// Place into arbitrary, but deterministic order.
	// We have to do this here,  because the input link l might not
	// have ever gone through an UnorderedLink constructor before.
	std::sort(_outgoing.begin(), _outgoing.end(), handle_less());
}

// ---------------------------------------------------------------

DEFINE_LINK_FACTORY(UnorderedLink, UNORDERED_LINK)

/* ===================== END OF FILE ===================== */
