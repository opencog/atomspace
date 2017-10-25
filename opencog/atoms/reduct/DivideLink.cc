/*
 * opencog/atoms/reduct/DivideLink.cc
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
#include <opencog/atoms/core/NumberNode.h>
#include "DivideLink.h"
#include "TimesLink.h"

using namespace opencog;

DivideLink::DivideLink(const HandleSeq& oset, Type t)
    : TimesLink(oset, t)
{
	init();
}

DivideLink::DivideLink(const Handle& a, const Handle& b)
    : TimesLink(DIVIDE_LINK, a, b)
{
	init();
}

DivideLink::DivideLink(Type t, const Handle& a, const Handle& b)
    : TimesLink(t, a, b)
{
	init();
}

DivideLink::DivideLink(const Link& l)
    : TimesLink(l)
{
	init();
}

void DivideLink::init(void)
{
	Type tscope = get_type();
	if (not classserver().isA(tscope, DIVIDE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a DivideLink");

	size_t sz = _outgoing.size();
	if (2 < sz or 0 == sz)
		throw InvalidParamException(TRACE_INFO,
			"Don't know how to divide that!");
}

Handle DivideLink::do_execute(AtomSpace* as, const HandleSeq& oset) const
{
	if (1 == oset.size())
	{
		NumberNodePtr na(unwrap_set(oset[0]));
		return createNumberNode(1.0 / na->get_value())->get_handle();
	}

	NumberNodePtr na(unwrap_set(oset[0]));
	NumberNodePtr nb(unwrap_set(oset[1]));
	return createNumberNode(na->get_value() / nb->get_value())->get_handle();
}

DEFINE_LINK_FACTORY(DivideLink, DIVIDE_LINK)

// ============================================================
