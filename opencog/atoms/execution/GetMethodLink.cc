/*
 * opencog/atoms/execution/GetMethodLink.cc
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

#include "GetMethodLink.h"

using namespace opencog;

static void check_type(const ValuePtr& value, const Type& type, const std::string& location)
{
	if (!nameserver().isA(value->get_type(), type))
	{
		throw SyntaxException(TRACE_INFO,
				"%s or subclass is expected as %s, actual parameter type is %s",
				nameserver().getTypeName(type),
				location,
				nameserver().getTypeName(value->get_type()));
	}
}

void GetMethodLink::check_outgoing_type(int index, const Type& type)
{
	std::string location = std::to_string(index) + " outgoing link of " + 
		nameserver().getTypeName(get_type());
	check_type(getOutgoingAtom(index), type, location);
}

GetMethodLink::GetMethodLink(const HandleSeq& output_set, Type type)
	: GroundedFunctionLink(output_set, type)
{
	check_outgoing_type(0, GROUNDED_OBJECT_NODE);
	check_outgoing_type(1, NODE);
}

GroundedObject* GetMethodLink::get_object() const
{
	return CastFromHandle<GroundedObjectNode>(getOutgoingAtom(0))->get_object();
}

const std::string& GetMethodLink::get_method_name() const
{
	return getOutgoingAtom(1)->get_name();
}

GroundedFunction GetMethodLink::get_function() const
{
	return get_object()->get_method(get_method_name());
}

auto GetMethodLinkCast = CastFromHandle<GetMethodLink>;

template<typename ... Args>
static GetMethodLinkPtr createGetMethodLink(Args&& ... args)
{
	return std::make_shared<GetMethodLink>(std::forward<Args>(args)...);
}

DEFINE_LINK_FACTORY(GetMethodLink, GET_METHOD_LINK)

