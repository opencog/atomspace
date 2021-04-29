/*
 * Pattern.cc
 *
 * Copyright (C) 2018 OpenCog Foundation
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

#include "Pattern.h"

namespace opencog {

std::string Pattern::to_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent << "Pattern: " << redex_name << std::endl;

	if (body)
		ss << indent << "PatternLink with body: "
		   << body->to_short_string() << std::endl;
	else
		ss << indent << "No pattern body" << std::endl;

	ss << indent << "pmandatory:\n" << oc_to_string(pmandatory) << std::endl;
	ss << indent << "absents:\n" << oc_to_string(absents) << std::endl;
	ss << indent << "always:\n" << oc_to_string(always) << std::endl;

	return ss.str();
}

// For gdb, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const Pattern& pattern, const std::string& indent)
{
	return pattern.to_string(indent);
}

} // ~namespace opencog
