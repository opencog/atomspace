/*
 * LinkColumn.cc
 *
 * Copyright (C) 2015, 2022, 2025 Linas Vepstas
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

#include <opencog/atoms/value/LinkValue.h>

#include "LinkColumn.h"

using namespace opencog;

LinkColumn::LinkColumn(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (not nameserver().isA(t, LINK_COLUMN))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a LinkColumn, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// Return a LinkValue vector.
ValuePtr LinkColumn::do_handle_loop(AtomSpace* as, bool silent,
                                    const HandleSeq& hseq)
{
	ValueSeq vseq;
	vseq.reserve(hseq.size());
	for (const Handle& h : hseq)
	{
		if (h->is_executable())
		{
			ValuePtr vp(h->execute(as, silent));
			vseq.emplace_back(vp);
		}
		else
			vseq.push_back(h);
	}

	return createLinkValue(std::move(vseq));
}

// ---------------------------------------------------------------

/// Return a LinkValue vector.
ValuePtr LinkColumn::do_execute(AtomSpace* as, bool silent)
{
	// If the given Atom is executable, then execute it.
	Handle base(_outgoing[0]);
	if (base->is_executable())
	{
		ValuePtr vpe(base->execute(as, silent));
		if (vpe->is_type(LINK_VALUE))
			return vpe;
		if (not vpe->is_atom())
			return createLinkValue(vpe);

		base = HandleCast(vpe);
	}

	// If we are here, then base is an link. Expect
	// it to contain things that evaluate to a double
	return do_handle_loop(as, silent, base->getOutgoingSet());
}

// ---------------------------------------------------------------

/// Return a FloatValue vector.
ValuePtr LinkColumn::execute(AtomSpace* as, bool silent)
{
	if (1 == _outgoing.size())
		return do_execute(as, silent);

	return do_handle_loop(as, silent, _outgoing);
}

DEFINE_LINK_FACTORY(LinkColumn, LINK_COLUMN)

/* ===================== END OF FILE ===================== */
