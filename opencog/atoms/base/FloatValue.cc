/*
 * opencog/atoms/base/FloatValue.cc
 *
 * Copyright (C) 2015 Linas Vepstas
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

#include <opencog/atoms/base/FloatValue.h>

using namespace opencog;

bool FloatValue::operator==(const ProtoAtom& other) const
{
	if (FLOAT_VALUE != other.getType()) return false;
	return true;
}

// ==============================================================

std::string FloatValue::toString(const std::string& indent)
{
	std::string rv = indent + "(FloatValue";
	for (double v :_value)
		rv += std::string(" ") + std::to_string(v);
	rv += ")";
	return rv;
}
