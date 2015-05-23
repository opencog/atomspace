/*
 * opencog/atoms/reduct/PutLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Put Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Put Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/ClassServer.h>
#include "PutLink.h"

using namespace opencog;

PutLink::PutLink(const HandleSeq& oset,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : FreeLink(PUT_LINK, oset, tv, av)
{
	init();
}

PutLink::PutLink(const Handle& a,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : FreeLink(PUT_LINK, a, tv, av)
{
	init();
}

PutLink::PutLink(Type t, const HandleSeq& oset,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : FreeLink(t, oset, tv, av)
{
	if (not classserver().isA(t, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

PutLink::PutLink(Type t, const Handle& a,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : FreeLink(t, a, tv, av)
{
	if (not classserver().isA(t, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

PutLink::PutLink(Type t, const Handle& a, const Handle& b,
                 TruthValuePtr tv,
                 AttentionValuePtr av)
    : FreeLink(t, a, b, tv, av)
{
	if (not classserver().isA(t, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

PutLink::PutLink(Link& l)
    : FreeLink(l)
{
	Type tscope = l.getType();
	if (not classserver().isA(tscope, PUT_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a PutLink");
	init();
}

void PutLink::init(void)
{
}

Handle PutLink::reduce(void)
{
   throw RuntimeException(TRACE_INFO, "Not reducible!");
}
