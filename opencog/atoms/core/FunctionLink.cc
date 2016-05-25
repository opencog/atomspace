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
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include "FunctionLink.h"

#include "ArityLink.h"
#include "DeleteLink.h"
// #include "MapLink.h"   goddamned python bindings
#include "SleepLink.h"
#include "TimeLink.h"
#include "RandomChoice.h"
#include "RandomNumber.h"

using namespace opencog;

void FunctionLink::init(void)
{
	FreeLink::init();
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

FunctionLink::FunctionLink(Type t, const Handle& a,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FreeLink(t, a, tv, av)
{
	if (not classserver().isA(t, FUNCTION_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FunctionLink");
	init();
}

FunctionLink::FunctionLink(Type t, const Handle& a, const Handle& b,
                   TruthValuePtr tv,
                   AttentionValuePtr av)
    : FreeLink(t, {a, b}, tv, av)
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
	throw RuntimeException(TRACE_INFO, "Not executable: %s\n",
		classserver().getTypeName(getType()).c_str());
}

Handle FunctionLink::do_execute(AtomSpace* as, const Handle& h)
{
	FunctionLinkPtr flp(factory(h));
	return flp->execute(as);
}

FunctionLinkPtr FunctionLink::factory(const Handle& h)
{
	// If h is of the right form already, its just a matter of calling
	// it.  Otherwise, we have to create
	FunctionLinkPtr flp(FunctionLinkCast(h));
	if (flp) return flp;

	if (nullptr == h)
		throw RuntimeException(TRACE_INFO, "Not executable!");

	return factory(h->getType(), h->getOutgoingSet());
}

// Basic type factory.
FunctionLinkPtr FunctionLink::factory(Type t, const HandleSeq& seq)
{
	if (ARITY_LINK == t)
		return createArityLink(seq);

	if (RANDOM_CHOICE_LINK == t)
		return createRandomChoiceLink(seq);

	if (RANDOM_NUMBER_LINK == t)
		return createRandomNumberLink(seq);

	if (SLEEP_LINK == t)
		return createSleepLink(seq);

	if (TIME_LINK == t)
		return createTimeLink(seq);

	// XXX FIXME In principle, we should manufacture the
	// ExecutionOutputLink as well. In practice, we can't, due to a
	// circular shared library dependency between python and itself.
	// (Python depends on ExecutionOutputLink and ExecutionOutputLink
	// depends on python. Whoops!)
	if (EXECUTION_OUTPUT_LINK == t)
		// return Handle(createExecutionOutputLink(seq));
		throw SyntaxException(TRACE_INFO, "Can't be a factory for this!");

	if (MAP_LINK == t)
		// return createMapLink(seq);
		throw SyntaxException(TRACE_INFO, "Can't be a factory for MapLink!");

	throw SyntaxException(TRACE_INFO,
		"FunctionLink is not a factory for %s",
		classserver().getTypeName(t).c_str());
}
