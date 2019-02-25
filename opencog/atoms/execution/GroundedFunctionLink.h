/*
 * opencog/atoms/execution/GroundedFunctionLink.h
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

#ifndef _OPENCOG_GROUNDED_FUNCTION_LINK_H
#define _OPENCOG_GROUNDED_FUNCTION_LINK_H

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/execution/GroundedObject.h>

namespace opencog
{

class GroundedFunctionLink : public Link
{
public:
	GroundedFunctionLink(const HandleSeq& output_set, Type type)
		: Link(output_set, type) { }
	virtual GroundedFunction get_function() const = 0;

};

using GroundedFunctionLinkPtr = std::shared_ptr<GroundedFunctionLink>;

}

#endif /* _OPENCOG_GROUNDED_FUNCTION_LINK_H */

