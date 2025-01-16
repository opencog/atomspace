/*
 * opencog/opencl/stream/OpenclFloatValue.cc
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

#include <opencog/util/exceptions.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/opencl/stream/OpenclFloatValue.h>

using namespace opencog;

bool OpenclFloatValue::operator==(const Value& other) const
{
	// Unlike Atoms, we are willing to compare othr types, as long
	// as the type hierarchy makes sense, and the values compare.
	if (not other.is_type(FLOAT_VALUE)) return false;

   const OpenclFloatValue* fov = (const OpenclFloatValue*) &other;

	if (_value.size() != fov->_value.size()) return false;
	size_t len = _value.size();
	for (size_t i=0; i<len; i++)
		// Compare floats with ULPS, because they are lexicographically
		// ordered. For technical explanation, see
		// http://www.cygnus-software.com/papers/comparingfloats/Comparing%20floating%20point%20numbers.htm
		// if (1.0e-15 < fabs(1.0 - fov->_value[i]/_value[i])) return false;
#define MAX_ULPS 24
		if (MAX_ULPS < llabs(*(int64_t*) &(_value[i]) - *(int64_t*)&(fov->_value[i])))
			return false;
	return true;
}

// ==============================================================

// Adds factory when the library is loaded.
DEFINE_VALUE_FACTORY(FLOAT_VALUE,
                     createOpenclFloatValue, std::vector<double>)
