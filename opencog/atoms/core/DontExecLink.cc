/*
 * DontExecLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
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

#include "DontExecLink.h"

using namespace opencog;

DontExecLink::DontExecLink(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	// Type must be as expected
	if (not nameserver().isA(t, DONT_EXEC_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting an DontExecLink, got %s", tname.c_str());
	}

	if (1 != oset.size())
		throw SyntaxException(TRACE_INFO,
			"DontExecLink expects only one argument");
}

DEFINE_LINK_FACTORY(DontExecLink, DONT_EXEC_LINK)

/* ===================== END OF FILE ===================== */
