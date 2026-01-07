/*
 * GuardLink.cc
 *
 * Copyright (C) 2026 BrainyBlaze Dyamics LLC
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
 */

#include <opencog/atoms/base/ClassServer.h>

#include "GuardLink.h"

using namespace opencog;

GuardLink::GuardLink(const HandleSeq&& oset, Type t)
	: ScopeLink(std::move(oset), t)
{
	if (not nameserver().isA(t, GUARD_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw SyntaxException(TRACE_INFO,
			"Expecting a GuardLink, got %s", tname.c_str());
	}
}

bool GuardLink::guard(const HandleSeq& varmap) const
{
	return true;
}

bool GuardLink::guard(const HandleMap& varmap) const
{
	return true;
}

DEFINE_LINK_FACTORY(GuardLink, GUARD_LINK)

/* ===================== END OF FILE ===================== */
