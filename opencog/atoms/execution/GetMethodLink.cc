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

GroundedObject& GetMethodLink::get_object() const
{
	return CastFromHandle<GroundedObjectNode>(getOutgoingAtom(0))->get_object();
}

const std::string& GetMethodLink::get_method_name() const
{
	return getOutgoingAtom(1)->get_name();
}

GroundedFunction GetMethodLink::get_function() const
{
	return get_object().get_method(get_method_name());
}

auto GetMethodLinkCast = CastFromHandle<GetMethodLink>;

template<typename ... Args>
static GetMethodLinkPtr createGetMethodLink(Args&& ... args)
{
	return std::make_shared<GetMethodLink>(std::forward<Args>(args)...);
}

DEFINE_LINK_FACTORY(GetMethodLink, GET_METHOD_LINK)

