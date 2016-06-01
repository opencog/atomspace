/*
 * opencog/atoms/base/AssoValue.cc
 *
 * Copyright (C) 2015, 2016 Linas Vepstas
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

#include <string>
#include <opencog/atoms/base/AssoValue.h>

using namespace opencog;

bool AssoValue::operator==(const ProtoAtom& other) const
{
	if (ASSO_VALUE != other.getType()) return false;
	return true;
}

// ==============================================================

std::string AssoValue::toString(const std::string& indent)
{
	std::string rv = indent + "(AssoValue (list";
	for (auto& pr :_map)
		rv += std::string("(list ") + pr.first->toString()
			+ pr.second->toString() + ")\n";
	rv += "))";
	return rv;
}
