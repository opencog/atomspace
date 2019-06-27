/*
 * opencog/atoms/value/Value.cc
 *
 * Copyright (C) 2018 SingularityNET Foundation
 * All Rights Reserved
 *
 * Author: Nil Geisweiller
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

#include "Value.h"

#include <sstream>

namespace opencog
{

std::string oc_to_string(const ValuePtr& vp, const std::string& indent)
{
	if (vp == nullptr)
		return indent + "null";
	return vp->to_string(indent);
}

std::string oc_to_string(const ValueSeq& vs, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << vs.size() << std::endl;
	size_t i = 0;
	for (const ValuePtr& vp : vs) {
		ss << indent << "value[" << i << "]:" << std::endl
		   << oc_to_string(vp, indent + oc_to_string_indent);
		i++;
	}
	return ss.str();
}

} // ~namespace std
