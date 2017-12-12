/*
 * opencog/atoms/core/ComposeLink.cc
 *
 * Copyright (C) 2017 Nil Geisweiller
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
#include "ComposeLink.h"

using namespace opencog;

void ComposeLink::check_type(Type t)
{
	if (not classserver().isA(t, COMPOSE_LINK))
		throw InvalidParamException(TRACE_INFO, "Expecting a ComposeLink");
}

ComposeLink::ComposeLink(const HandleSeq oset, Type t) : FunctionLink(oset, t)
{
	check_type(t);
}

ComposeLink::ComposeLink(const Link& l) : FunctionLink(l)
{
	check_type(l.get_type());
}

Handle ComposeLink::execute(AtomSpace* as) const
{
	OC_ASSERT(false, "TODO");
	return Handle::UNDEFINED;
}

DEFINE_LINK_FACTORY(ComposeLink, COMPOSE_LINK);
