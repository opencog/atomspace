/*
 * opencog/atoms/value/FloatValue.cc
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
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

ValuePtr FloatValue::value_at_index(size_t idx) const
{
	double d = 0.0;
	if (_value.size() > idx) d = _value[idx];
	return createFloatValue(d);
}

ValuePtr FloatValue::incrementCount(const std::vector<double>& v) const
{
	// Make a copy
	std::vector<double> new_vect = _value;

	// Increase size to fit.
	if (new_vect.size() < v.size())
		new_vect.resize(v.size(), 0.0);

	// Increment
	for (size_t idx=0; idx < new_vect.size(); idx++)
		new_vect[idx] += v[idx];

	// Return a brand new value of the same type.
	return createFloatValue(_type, new_vect);
}

ValuePtr FloatValue::incrementCount(size_t idx, double count) const
{
	// Make a copy
	std::vector<double> new_vect = _value;

	// Increase size to fit.
	if (new_vect.size() <= idx)
		new_vect.resize(idx+1, 0.0);

	// Increment
	new_vect[idx] += count;

	// Return a brand new value of the same type.
	return createFloatValue(_type, new_vect);
}

bool FloatValue::operator==(const Value& other) const
{
	// Unlike Atoms, we are willing to compare othr types, as long
	// as the type hierarchy makes sense, and the values compare.
	if (not other.is_type(FLOAT_VALUE)) return false;

   const FloatValue* fov = (const FloatValue*) &other;

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

std::string FloatValue::to_string(const std::string& indent, Type t) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(t);
	SAFE_UPDATE(rv,
	{
		for (double v :_value)
		{
			char buf[40];
			snprintf(buf, 40, "%.16g", v);
			rv += std::string(" ") + buf;
		}
	});

	rv += ")";
	return rv;
}

// ==============================================================

/// Scalar addition
std::vector<double> opencog::plus(double scalar, const std::vector<double>& fv)
{
	size_t len = fv.size();
	std::vector<double> sum(len);
	for (size_t i=0; i<len; i++)
		sum[i] = scalar + fv[i];

	return sum;
}

/// Scalar subtraction
std::vector<double> opencog::minus(double scalar, const std::vector<double>& fv)
{
	size_t len = fv.size();
	std::vector<double> diff(len);
	for (size_t i=0; i<len; i++)
		diff[i] = scalar - fv[i];

	return diff;
}

std::vector<double> opencog::minus(const std::vector<double>& fv, double scalar)
{
	size_t len = fv.size();
	std::vector<double> diff(len);
	for (size_t i=0; i<len; i++)
		diff[i] = fv[i] - scalar;

	return diff;
}

/// Scalar multiplication
std::vector<double> opencog::times(double scalar, const std::vector<double>& fv)
{
	size_t len = fv.size();
	std::vector<double> prod(len);
	for (size_t i=0; i<len; i++)
		prod[i] = scalar * fv[i];

	return prod;
}

/// Scalar division
std::vector<double> opencog::divide(double scalar, const std::vector<double>& fv)
{
	size_t len = fv.size();
	std::vector<double> ratio(len);
	for (size_t i=0; i<len; i++)
		ratio[i] = scalar / fv[i];

	return ratio;
}

/// Vector (point-wise) addition
/// The shorter vector is assumed to be zero-padded.
std::vector<double> opencog::plus(const std::vector<double>& fva,
                                  const std::vector<double>& fvb)
{
	size_t lena = fva.size();
	size_t lenb = fvb.size();

	if (1 == lena)
		return plus(fva[0], fvb);

	if (1 == lenb)
		return plus(fvb[0], fva);

	std::vector<double> sum(std::max(lena, lenb));
	if (lena < lenb)
	{
		size_t i=0;
		for (; i<lena; i++)
			sum[i] = fva[i] + fvb[i];
		for (; i<lenb; i++)
			sum[i] = fvb[i];
	}
	else
	{
		size_t i=0;
		for (; i<lenb; i++)
			sum[i] = fva[i] + fvb[i];
		for (; i<lena; i++)
			sum[i] = fva[i];
	}
	return sum;
}

/// Vector (point-wise) subtraction
/// The shorter vector is assumed to be zero-padded.
std::vector<double> opencog::minus(const std::vector<double>& fva,
                                   const std::vector<double>& fvb)
{
	size_t lena = fva.size();
	size_t lenb = fvb.size();

	if (1 == lena)
		return minus(fva[0], fvb);

	if (1 == lenb)
		return minus(fva, fvb[0]);

	std::vector<double> diff(std::max(lena, lenb));
	if (lena < lenb)
	{
		size_t i=0;
		for (; i<lena; i++)
			diff[i] = fva[i] - fvb[i];
		for (; i<lenb; i++)
			diff[i] = -fvb[i];
	}
	else
	{
		size_t i=0;
		for (; i<lenb; i++)
			diff[i] = fva[i] - fvb[i];
		for (; i<lena; i++)
			diff[i] = fva[i];
	}
	return diff;
}

/// Vector (point-wise) multiplication
/// The shorter vector is assumed to be one-padded.
/// Unless the shorter vector is a scalar, in which case we do scalar
/// multiplication. This is the "right thing to do", because that
/// is the general user intent.  We could detect this case in all
/// the callers to this routine, or we could just handle it here.
/// This may seem messy to you, but this is the easiest solution.
std::vector<double> opencog::times(const std::vector<double>& fva,
                                   const std::vector<double>& fvb)
{
	size_t lena = fva.size();
	size_t lenb = fvb.size();

	std::vector<double> prod(std::max(lena, lenb));
	if (1 == lena)
	{
		double f = fva[0];
		for (size_t i=0; i<lenb; i++)
			prod[i] = f * fvb[i];
	}
	else
	if (1 == lenb)
	{
		double f = fvb[0];
		for (size_t i=0; i<lena; i++)
			prod[i] = f * fva[i];
	}
	else
	if (lena < lenb)
	{
		size_t i=0;
		for (; i<lena; i++)
			prod[i] = fva[i] * fvb[i];
		for (; i<lenb; i++)
			prod[i] = fvb[i];
	}
	else
	{
		size_t i=0;
		for (; i<lenb; i++)
			prod[i] = fva[i] * fvb[i];
		for (; i<lena; i++)
			prod[i] = fva[i];
	}
	return prod;
}

/// Vector (point-wise) division
/// The shorter vector is assumed to be one-padded.
/// If the shorter vector has length one, assume its a scalar.
/// See comments on times() above about scalars.
std::vector<double> opencog::divide(const std::vector<double>& fva,
                                    const std::vector<double>& fvb)
{
	size_t lena = fva.size();
	size_t lenb = fvb.size();

	std::vector<double> ratio(std::max(lena, lenb));
	if (1 == lena)
	{
		double f = fva[0];
		for (size_t i=0; i<lenb; i++)
			ratio[i] = f / fvb[i];
	}
	else
	if (1 == lenb)
	{
		double f = fvb[0];
		for (size_t i=0; i<lena; i++)
			ratio[i] = fva[i] / f;
	}
	else
	if (lena < lenb)
	{
		size_t i=0;
		for (; i<lena; i++)
			ratio[i] = fva[i] / fvb[i];
		for (; i<lenb; i++)
			ratio[i] = 1.0 / fvb[i];
	}
	else
	{
		size_t i=0;
		for (; i<lenb; i++)
			ratio[i] = fva[i] / fvb[i];
		for (; i<lena; i++)
			ratio[i] = fva[i];
	}
	return ratio;
}

// Adds factory when the library is loaded.
DEFINE_VALUE_FACTORY(FLOAT_VALUE,
                     createFloatValue, std::vector<double>)
DEFINE_VALUE_FACTORY(FLOAT_VALUE,
                     createFloatValue, double)
