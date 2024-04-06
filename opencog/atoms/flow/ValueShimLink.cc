/*
 * ValueShimLink.cc
 *
 * Copyright (C) 2015, 2018, 2024 Linas Vepstas
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

#include "ValueShimLink.h"

using namespace opencog;

ValueShimLink::ValueShimLink(Type t)
	: Link(t)
{
}

ValueShimLink::ValueShimLink(const HandleSeq& oset, Type t)
	: Link(t)
{
	if (0 != oset.size())
		throw RuntimeException(TRACE_INFO, "Non-empty outgoing set not allowed!");
}

void ValueShimLink::setAtomSpace(AtomSpace *)
{
	throw RuntimeException(TRACE_INFO, "Cannot be placed into AtomSpace!");
}

std::string ValueShimLink::to_short_string(const std::string& indent) const
{
	std::string more_indent = indent + "  ";
	return indent + "(ValueShim\n" + val->to_string(more_indent) + ")";
}

DEFINE_LINK_FACTORY(ValueShimLink, VALUE_SHIM_LINK)

/* ===================== END OF FILE ===================== */
