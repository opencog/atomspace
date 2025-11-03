/*
 * DrainLink.cc
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, Inc.
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  November 2025
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

#include <opencog/atoms/value/VoidValue.h>
#include <opencog/atoms/value/LinkValue.h>

#include "DrainLink.h"

using namespace opencog;

DrainLink::DrainLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (not nameserver().isA(t, DRAIN_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a DrainLink, got %s", tname.c_str());
	}

	if (0 == _outgoing.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting at least one executable argument!");

	for (const Handle& ho : _outgoing)
		if (not ho->is_executable())
			throw SyntaxException(TRACE_INFO,
				"Expecting an executable argument; got %s",
				ho->to_string().c_str());
}

// ---------------------------------------------------------------

/// Execute the drain operation.
ValuePtr DrainLink::execute(AtomSpace* as, bool silent)
{
	while (true)
	{
		for (const Handle& ho : _outgoing)
		{
			ValuePtr vp = ho->execute(as, silent);
			if (nullptr == vp)
				return createVoidValue();
			if (VOID_VALUE == vp->get_type())
				return vp;
			if (LINK_VALUE == vp->get_type() and
		      LinkValueCast(vp)->size() == 0)
				return vp;
		}
	}
	return nullptr; // Not reached.
}

DEFINE_LINK_FACTORY(DrainLink, DRAIN_LINK)

/* ===================== END OF FILE ===================== */
