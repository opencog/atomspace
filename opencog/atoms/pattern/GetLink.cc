/*
 * GetLink.cc
 *
 * Copyright (C) 2019 Linas Vepstas
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/query/BindLinkAPI.h>

#include "GetLink.h"

using namespace opencog;

void GetLink::init(void)
{
	Type t = get_type();
	if (not nameserver().isA(t, GET_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a GetLink, got %s", tname.c_str());
	}
}

GetLink::GetLink(const HandleSeq& hseq, Type t)
	: PatternLink(hseq, t)
{
	init();
}

GetLink::GetLink(const Link &l)
	: PatternLink(l)
{
	init();
}

/* ================================================================= */

Handle GetLink::execute(AtomSpace* as, bool silent)
{
	// XXX Someday, copy over the code from Satisfier.cc to here.
	return satisfying_set(as, get_handle());
}

DEFINE_LINK_FACTORY(GetLink, GET_LINK)

/* ===================== END OF FILE ===================== */
