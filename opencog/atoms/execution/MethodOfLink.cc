/*
 * opencog/atoms/execution/MethodOfLink.cc
 *
 * Copyright (C) 2019 OpenCog Foundation
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

#include <opencog/atoms/execution/GroundedObjectNode.h>

#include "MethodOfLink.h"

using namespace opencog;

MethodOfLink::MethodOfLink(const HandleSeq& output_set, Type type)
	: GroundedFunctionLink(output_set, type)
{
	check_outgoing_type(0, GROUNDED_OBJECT_NODE);
	check_outgoing_type(1, NODE);
}

GroundedObject* MethodOfLink::get_object() const
{
	return CastFromHandle<GroundedObjectNode>(getOutgoingAtom(0))->get_object();
}

const std::string& MethodOfLink::get_method_name() const
{
	return getOutgoingAtom(1)->get_name();
}

GroundedFunction MethodOfLink::get_function() const
{
	return get_object()->get_method(get_method_name());
}

auto MethodOfLinkCast = CastFromHandle<MethodOfLink>;

template<typename ... Args>
static MethodOfLinkPtr createMethodOfLink(Args&& ... args)
{
	return std::make_shared<MethodOfLink>(std::forward<Args>(args)...);
}

DEFINE_LINK_FACTORY(MethodOfLink, METHOD_OF_LINK)

