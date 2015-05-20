/*
 * opencog/atoms/reduct/TimesLink.cc
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Times Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Times Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/ClassServer.h>
#include <opencog/atoms/NumberNode.h>
#include "TimesLink.h"

using namespace opencog;

TimesLink::TimesLink(const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : ArithmeticLink(TIMES_LINK, oset, tv, av)
{
	init();
}

TimesLink::TimesLink(Type t, const HandleSeq& oset,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : ArithmeticLink(t, oset, tv, av)
{
	if (not classserver().isA(t, TIMES_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a TimesLink");
	init();
}

TimesLink::TimesLink(Type t, const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : ArithmeticLink(t, a, b, tv, av)
{
	if (not classserver().isA(t, TIMES_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a TimesLink");
	init();
}

TimesLink::TimesLink(const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : ArithmeticLink(TIMES_LINK, a, b, tv, av)
{
	init();
}

TimesLink::TimesLink(Link& l)
    : ArithmeticLink(l)
{
	Type tscope = l.getType();
	if (not classserver().isA(tscope, TIMES_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a TimesLink");
	init();
}

double TimesLink::konsd(double a, double b) const { return a*b; }

void TimesLink::init(void)
{
	knild = 1.0;
}
