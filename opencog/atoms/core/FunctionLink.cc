/*
 * opencog/atoms/core/FunctionLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include "FunctionLink.h"

using namespace opencog;

FunctionLink::FunctionLink(const HandleSeq&& oset, Type t)
    : Link(std::move(oset), t)
{
	if (FUNCTION_LINK == t)
		throw InvalidParamException(TRACE_INFO,
			"FunctionLinks are private and cannot be instantiated.");
	if (not nameserver().isA(t, FUNCTION_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FunctionLink");
}

DEFINE_LINK_FACTORY(FunctionLink, FUNCTION_LINK);

/* ===================== END OF FILE ===================== */
