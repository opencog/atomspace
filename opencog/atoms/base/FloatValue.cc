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
	if (FLOAT_VALUE != other.get_type()) return false;

   const FloatValue* fov = (const FloatValue*) &other;

	if (_value.size() != fov->_value.size()) return false;
	size_t len = _value.size();
	for (size_t i=0; i<len; i++)
		// Compare floats with ULPS, because they are lexicographically
		// ordered. For technical explanation, see
		// http://www.cygnus-software.com/papers/comparingfloats/Comparing%20floating%20point%20numbers.htm
		// if (1.0e-15 < fabs(1.0 - fov->_value[i]/_value[i])) return false;
		//
		// Beats me why, but the ValueSaveUTest requires ULPS of 11 to
		// pass, which works out to about 2.3e-15 in practice.
#define MAX_ULPS 24
		if (MAX_ULPS < abs(*(int64_t*) &(_value[i]) - *(int64_t*)&(fov->_value[i])))
			return false;
	return true;
}

// ==============================================================

std::string FloatValue::to_string(const std::string& indent) const
{
	std::string rv = indent + "(FloatValue";
	for (double v :_value)
	{
		char buf[40];
		snprintf(buf, 40, "%20.17g", v);
		rv += std::string(" ") + buf;
	}
	rv += ")\n";
	return rv;
}
