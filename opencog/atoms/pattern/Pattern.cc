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
	if (not clauses.empty())
		ss << indent << "clauses:" << std::endl
		   << oc_to_string(clauses, indent + OC_TO_STRING_INDENT);
	if (not constants.empty())
		ss << indent << "constants:" << std::endl
		   << oc_to_string(constants, indent + OC_TO_STRING_INDENT);
	if (not mandatory.empty())
		ss << indent << "mandatory:" << std::endl
		   << oc_to_string(mandatory, indent + OC_TO_STRING_INDENT);
	if (not optionals.empty())
		ss << indent << "optionals:" << std::endl
		   << oc_to_string(optionals, indent + OC_TO_STRING_INDENT);
	if (not black.empty())
		ss << indent << "black:" << std::endl
		   << oc_to_string(black, indent + OC_TO_STRING_INDENT);
	if (not evaluatable_terms.empty())
		ss << indent << "evaluatable_terms:" << std::endl
		   << oc_to_string(evaluatable_terms,
		                   indent + OC_TO_STRING_INDENT);
	if (not evaluatable_holders.empty())
		ss << indent << "evaluatable_holders:" << std::endl
		   << oc_to_string(evaluatable_holders,
		                   indent + OC_TO_STRING_INDENT);
	if (not executable_terms.empty())
		ss << indent << "executable_terms:" << std::endl
		   << oc_to_string(executable_terms,
		                   indent + OC_TO_STRING_INDENT);
	if (not executable_holders.empty())
		ss << indent << "executable_holders:" << std::endl
		   << oc_to_string(executable_holders,
		                   indent + OC_TO_STRING_INDENT);
	return ss.str();
}

// For gdb, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const Pattern& pattern, const std::string& indent)
{
	return pattern.to_string(indent);
}

} // ~namespace opencog
