/*
 * SplitLink.cc
 *
 * Copyright (C) 2015, 2022, 2024 Linas Vepstas
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
#include <opencog/atoms/value/LinkValue.h>

#include "SplitLink.h"

using namespace opencog;

SplitLink::SplitLink(const HandleSeq&& oset, Type t)
	: CollectionOfLink(std::move(oset), t)
{
	if (not nameserver().isA(t, SPLIT_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an SplitLink, got %s", tname.c_str());
	}

	if (not _have_typespec)
	{
		_out_type = LINK_VALUE;
		_out_is_link = false;
	}

	// Split on whitespace
	_sep = " \t\n\r\v";
}

// ---------------------------------------------------------------

/// Split a Node name or a StringValue
ValuePtr SplitLink::execute(AtomSpace* as, bool silent)
{
	int coff = 0;
	if (_have_typespec) coff = 1;

	// If the given Atom is not executable, then it is just
	// a node. Split the node name.
	Handle base(_outgoing[coff]);
	if (not base->is_executable())
		return split(as, base);

	// Same as above, but this time, we execute, and branch
	// over all the various possibilities.
	ValuePtr vp = base->execute(as, silent);

	// Hmmm. FilterLink returns null pointer when the filter
	// is empty. Is this a bug in FilterLink? Not sure any more.
	if (nullptr == vp)
	{
		if (_out_is_link)
			return createLink(_out_type);
		return createLinkValue(_out_type, ValueSeq({}));
	}

	if (vp->is_atom())
		return split(as, HandleCast(vp));

	return splat(vp);
}

DEFINE_LINK_FACTORY(SplitLink, SPLIT_LINK)

/* ===================== END OF FILE ===================== */
