/*
 * opencog/atoms/execution/DotLink.cc
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

#include "DotLink.h"

using namespace opencog;

GroundedObject& DotLink::get_object() const
{
	return CastFromHandle<GroundedObjectNode>(getOutgoingAtom(0))->get_object();
}

const std::string& DotLink::get_method_name() const
{
	return getOutgoingAtom(1)->get_name();
}

GroundedFunction DotLink::get_function() const
{
	return get_object().get_method(get_method_name());
}

auto DotLinkCast = CastFromHandle<DotLink>;

template<typename ... Args>
static DotLinkPtr createDotLink(Args&& ... args)
{
	return createType<DotLink>(std::forward<Args>(args)...);
}

DEFINE_LINK_FACTORY(DotLink, DOT_LINK)

