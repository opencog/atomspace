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

using namespace opencog;

void FunctionLink::init(void)
{
	FreeLink::init();
}

FunctionLink::FunctionLink(const HandleSeq& oset, Type t)
    : FreeLink(oset, t)
{
	if (not classserver().isA(t, FUNCTION_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FunctionLink");
	init();
}

FunctionLink::FunctionLink(Type t, const Handle& a)
    : FreeLink(t, a)
{
	if (not classserver().isA(t, FUNCTION_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FunctionLink");
	init();
}

FunctionLink::FunctionLink(Type t, const Handle& a, const Handle& b)
    : FreeLink({a, b}, t)
{
	if (not classserver().isA(t, FUNCTION_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FunctionLink");
	init();
}

FunctionLink::FunctionLink(const Link& l)
    : FreeLink(l)
{
	Type tscope = l.get_type();
	if (not classserver().isA(tscope, FUNCTION_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a FunctionLink");
	init();
}

Handle FunctionLink::execute(AtomSpace* as) const
{
	throw RuntimeException(TRACE_INFO, "Not executable: %s\n",
		classserver().getTypeName(get_type()).c_str());
}

Handle FunctionLink::do_execute(AtomSpace* as, const Handle& h)
{
	FunctionLinkPtr flp(castfactory(h));
	return flp->execute(as);
}

FunctionLinkPtr FunctionLink::castfactory(const Handle& h)
{
	// If h is of the right form already, its just a matter of calling
	// it.  Otherwise, we have to create
	FunctionLinkPtr flp(FunctionLinkCast(h));
	if (flp) return flp;

	if (nullptr == h)
		throw RuntimeException(TRACE_INFO, "Not executable!");

	auto fact = classserver().getFactory(h->get_type());
	return FunctionLinkCast((*fact)(h));
}

DEFINE_LINK_FACTORY(FunctionLink, FUNCTION_LINK);

/* ===================== END OF FILE ===================== */
