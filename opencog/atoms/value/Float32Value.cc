/*
 * opencog/atoms/value/Float32Value.cc
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics Inc.
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
#include <opencog/atoms/value/Float32Value.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

ValuePtr Float32Value::incrementCount(const std::vector<float>& v) const
{
	// Make a copy
	std::vector<float> new_vect = _value;

	// Increase size to fit.
	if (new_vect.size() < v.size())
		new_vect.resize(v.size(), 0.0f);

	// Increment
	for (size_t idx=0; idx < new_vect.size(); idx++)
		new_vect[idx] += v[idx];

	// Return a brand new value of the same type.
	return valueserver().create(_type, std::move(new_vect));
}

bool Float32Value::operator==(const Value& other) const
{
	// Unlike Atoms, we are willing to compare other types, as long
	// as the type hierarchy makes sense, and the values compare.
	// XXX Except that, currently, we do not compare to the 64-bit doubles.
	// Perhaps we should, someday.
	if (not other.is_type(FLOAT32_VALUE)) return false;

	const Float32Value* fov = (const Float32Value*) &other;
	if (_value.size() != fov->_value.size()) return false;
	size_t len = _value.size();
	for (size_t i=0; i<len; i++)
	{
		// Compare floats with ULPS, because they are lexicographically
		// ordered. For technical explanation, see
		// http://www.cygnus-software.com/papers/comparingfloats/Comparing%20floating%20point%20numbers.htm

		// For 32-bit floats, we use int32_t instead of int64_t
		int32_t self = *(int32_t*) &(_value[i]);
		int32_t other= *(int32_t*) &(fov->_value[i]);
		int32_t lili = self - other;
		if (0L > lili) lili = -lili;
		uint32_t lulu = (uint32_t) lili;

		// For 32-bit floats, we use a smaller ULPS threshold
		// Since we have less precision, we need fewer ULPS
		// 12 ULPS is approximately one part in 1e-6
#define MAX_ULPS_32 12UL
		if (MAX_ULPS_32 < lulu) return false;
	}
	return true;
}

// ==============================================================

std::string Float32Value::to_string(const std::string& indent, Type t) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(t);
	SAFE_UPDATE(rv,
	{
		for (float v :_value)
		{
			char buf[40];
			// 32-bit floats have a 23-bit mantissa, so about 7.2 bits
			// of precision. Round up and print 8 decimal places.
			snprintf(buf, 40, "%.8g", v);
			rv += std::string(" ") + buf;
		}
	});

	rv += ")";
	return rv;
}

// ==============================================================

/// Scalar addition
std::vector<float> opencog::plus(float scalar, const std::vector<float>& fv)
{
	size_t len = fv.size();
	std::vector<float> sum(len);
	for (size_t i=0; i<len; i++)
		sum[i] = scalar + fv[i];

	return sum;
}

/// Scalar subtraction
std::vector<float> opencog::minus(float scalar, const std::vector<float>& fv)
{
	size_t len = fv.size();
	std::vector<float> diff(len);
	for (size_t i=0; i<len; i++)
		diff[i] = scalar - fv[i];

	return diff;
}

std::vector<float> opencog::minus(const std::vector<float>& fv, float scalar)
{
	size_t len = fv.size();
	std::vector<float> diff(len);
	for (size_t i=0; i<len; i++)
		diff[i] = fv[i] - scalar;

	return diff;
}

/// Scalar multiplication
std::vector<float> opencog::times(float scalar, const std::vector<float>& fv)
{
	size_t len = fv.size();
	std::vector<float> prod(len);
	for (size_t i=0; i<len; i++)
		prod[i] = scalar * fv[i];

	return prod;
}

/// Scalar division
std::vector<float> opencog::divide(float scalar, const std::vector<float>& fv)
{
	size_t len = fv.size();
	std::vector<float> ratio(len);
	for (size_t i=0; i<len; i++)
		ratio[i] = scalar / fv[i];

	return ratio;
}

/// Vector (point-wise) addition
/// The shorter vector is assumed to be zero-padded.
std::vector<float> opencog::plus(const std::vector<float>& fva,
                                  const std::vector<float>& fvb)
{
	size_t lena = fva.size();
	size_t lenb = fvb.size();

	if (1 == lena)
		return plus(fva[0], fvb);

	if (1 == lenb)
		return plus(fvb[0], fva);

	std::vector<float> sum(std::max(lena, lenb));
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
std::vector<float> opencog::minus(const std::vector<float>& fva,
                                   const std::vector<float>& fvb)
{
	size_t lena = fva.size();
	size_t lenb = fvb.size();

	if (1 == lena)
		return minus(fva[0], fvb);

	if (1 == lenb)
		return minus(fva, fvb[0]);

	std::vector<float> diff(std::max(lena, lenb));
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
std::vector<float> opencog::times(const std::vector<float>& fva,
                                   const std::vector<float>& fvb)
{
	size_t lena = fva.size();
	size_t lenb = fvb.size();

	std::vector<float> prod(std::max(lena, lenb));
	if (1 == lena)
	{
		float f = fva[0];
		for (size_t i=0; i<lenb; i++)
			prod[i] = f * fvb[i];
	}
	else
	if (1 == lenb)
	{
		float f = fvb[0];
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
std::vector<float> opencog::divide(const std::vector<float>& fva,
                                    const std::vector<float>& fvb)
{
	size_t lena = fva.size();
	size_t lenb = fvb.size();

	std::vector<float> ratio(std::max(lena, lenb));
	if (1 == lena)
	{
		float f = fva[0];
		for (size_t i=0; i<lenb; i++)
			ratio[i] = f / fvb[i];
	}
	else
	if (1 == lenb)
	{
		float f = fvb[0];
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
			ratio[i] = 1.0f / fvb[i];
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
DEFINE_VALUE_FACTORY(FLOAT32_VALUE,
                     createFloat32Value, std::vector<float>)
