/*
 * opencog/atoms/reduct/FunctionLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Function Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Function Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/ClassServer.h>
#include "FunctionLink.h"

using namespace opencog;

FunctionLink::FunctionLink(const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FreeLink(FUNCTION_LINK, oset, tv, av)
{
}

FunctionLink::FunctionLink(const Handle& a,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FreeLink(FUNCTION_LINK, a, tv, av)
{
}

FunctionLink::FunctionLink(Type t, const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FreeLink(t, oset, tv, av)
{
	if (not classserver().isA(t, FUNCTION_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FunctionLink");
	init();
}

FunctionLink::FunctionLink(Type t, const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FreeLink(t, a, b, tv, av)
{
	if (not classserver().isA(t, FUNCTION_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FunctionLink");
	init();
}

FunctionLink::FunctionLink(Link& l)
    : FreeLink(l)
{
	Type tscope = l.getType();
	if (not classserver().isA(tscope, FUNCTION_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FunctionLink");
	init();
}

Handle FunctionLink::execute(AtomSpace* as) const
{
	throw RuntimeException(TRACE_INFO, "Not executable!");
}
