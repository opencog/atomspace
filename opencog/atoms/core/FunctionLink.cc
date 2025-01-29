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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/DefineLink.h>

#include "FunctionLink.h"

using namespace opencog;

void FunctionLink::check_type(Type t)
{
	if (FUNCTION_LINK == t)
		throw InvalidParamException(TRACE_INFO,
			"FunctionLinks are private and cannot be instantiated.");
	if (not nameserver().isA(t, FUNCTION_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FunctionLink");
}

void FunctionLink::init(void)
{
	FreeLink::init();
}

FunctionLink::FunctionLink(const HandleSeq&& oset, Type t)
    : FreeLink(std::move(oset), t)
{
	check_type(t);
	init();
}

// ===========================================================

/// Generic utility -- execute the argument, and return the result
/// of the execution.
ValuePtr FunctionLink::get_value(AtomSpace* as, bool silent, ValuePtr vptr)
{
	if (vptr->is_type(DEFINED_PROCEDURE_NODE))
	{
		vptr = DefineLink::get_definition(HandleCast(vptr));
	}
	while (vptr->is_atom())
	{
		Handle h(HandleCast(vptr));
		if (not h->is_executable()) break;

		ValuePtr red(h->execute(as, silent));

		// It would probably be better to throw a silent exception, here?
		if (nullptr == red) return vptr;
		if (*red == *vptr) return vptr;
		vptr = red;

		// The executable function might be a GetLink, which returns
		// a SetLink of results. If the SetLink is wrapping only one
		// atom, then unwrap it and return that value. If it contains
		// more than one atom, we don't know what to do.
		if (SET_LINK == vptr->get_type())
		{
			Handle setl(HandleCast(vptr));
			if (1 == setl->get_arity())
				vptr = setl->getOutgoingAtom(0);
		}
	}
	return vptr;
}

DEFINE_LINK_FACTORY(FunctionLink, FUNCTION_LINK);

/* ===================== END OF FILE ===================== */
