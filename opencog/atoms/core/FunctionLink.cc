/*
 * opencog/atoms/reduct/FunctionLink.cc
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

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/ClassServer.h>
#include "FunctionLink.h"

#include "ArityLink.h"
#include "DeleteLink.h"
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
	// If h is of the right form already, its just a matter of calling
	// it.  Otherwise, we have to create
	FunctionLinkPtr flp(FunctionLinkCast(factory(LinkCast(h))));
	if (NULL == flp)
		throw RuntimeException(TRACE_INFO, "Not executable!");

	return flp->execute(as);
}

LinkPtr FunctionLink::factory(LinkPtr lp)
{
	if (NULL == lp)
		throw RuntimeException(TRACE_INFO, "Not executable!");

	// If h is of the right form already, its just a matter of calling
	// it.  Otherwise, we have to create
	FunctionLinkPtr flp(FunctionLinkCast(lp));
	if (flp) return lp;

	return LinkCast(factory(lp->getType(), lp->getOutgoingSet()));
}

// Basic type factory.
Handle FunctionLink::factory(Type t, const HandleSeq& seq)
{
	if (ARITY_LINK == t)
		return Handle(createArityLink(seq));

	if (RANDOM_CHOICE_LINK == t)
		return Handle(createRandomChoiceLink(seq));

	if (RANDOM_NUMBER_LINK == t)
		return Handle(createRandomNumberLink(seq));

	if (TIME_LINK == t)
		return Handle(createTimeLink(seq));

	// XXX FIXME In principle, we should manufacture the
	// ExecutionOutputLink as well. In practice, we can't, due to a
	// circular shared library dependency between python and itself.
	// (Python depends on ExecutionOutputLink and ExecutionOutputLink
	// depends on python. Whoops!)
	if (EXECUTION_OUTPUT_LINK == t)
		// return Handle(createExecutionOutputLink(seq));
		throw SyntaxException(TRACE_INFO, "Can't be a factory for this!");

	throw SyntaxException(TRACE_INFO,
		"FunctionLink is not a factory for %s",
		classserver().getTypeName(t).c_str());
}
