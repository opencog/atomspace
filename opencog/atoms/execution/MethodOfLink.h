/*
 * opencog/atoms/execution/MethodOfLink.h
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

#ifndef _OPENCOG_METHOD_OF_LINK_H
#define _OPENCOG_METHOD_OF_LINK_H

#include <opencog/atoms/execution/GroundedFunctionLink.h>

namespace opencog
{

class MethodOfLink : public GroundedFunctionLink
{
private:
	GroundedObject* get_object() const;
	const std::string& get_method_name() const;

public:
	MethodOfLink(const HandleSeq& output_set, Type type);
	virtual GroundedFunction get_function() const;

	static Handle factory(const Handle&);
};

using MethodOfLinkPtr = std::shared_ptr<MethodOfLink>;

}

#endif /* _OPENCOG_METHOD_OF_LINK_H */

