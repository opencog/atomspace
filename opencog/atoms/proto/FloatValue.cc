/*
 * opencog/atoms/proto/FloatValue.cc
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
#include <opencog/atoms/proto/FloatValue.h>

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
#define MAX_ULPS 24
		if (MAX_ULPS < llabs(*(int64_t*) &(_value[i]) - *(int64_t*)&(fov->_value[i])))
			return false;
	return true;
}

// ==============================================================

std::string FloatValue::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	for (double v :_value)
	{
		char buf[40];
		snprintf(buf, 40, "%.17g", v);
		rv += std::string(" ") + buf;
	}
	rv += ")\n";
	return rv;
}

// ==============================================================

/// Scalar multiplication
ProtoAtomPtr opencog::times(double scalar, const FloatValuePtr& fvp)
{
	const std::vector<double>& fv = fvp->value();
	size_t len = fv.size();
	std::vector<double> prod(len);
	for (size_t i=0; i<len; i++)
		prod[i] = scalar * fv[i];

	return createFloatValue(prod);
}

/// Scalar addition
ProtoAtomPtr opencog::plus(double scalar, const FloatValuePtr& fvp)
{
	const std::vector<double>& fv = fvp->value();
	size_t len = fv.size();
	std::vector<double> sum(len);
	for (size_t i=0; i<len; i++)
		sum[i] = scalar + fv[i];

	return createFloatValue(sum);
}

/// Scalar division
ProtoAtomPtr opencog::divide(double scalar, const FloatValuePtr& fvp)
{
	const std::vector<double>& fv = fvp->value();
	size_t len = fv.size();
	std::vector<double> ratio(len);
	for (size_t i=0; i<len; i++)
		ratio[i] = scalar / fv[i];

	return createFloatValue(ratio);
}

/// Vector (point-wise) multiplication
ProtoAtomPtr opencog::times(const FloatValuePtr& fvpa, const FloatValuePtr& fvpb)
{
	const std::vector<double>& fva = fvpa->value();
	const std::vector<double>& fvb = fvpb->value();
	size_t len = fva.size();
	if (len != fvb.size())
		throw RuntimeException(TRACE_INFO, "Mismatched vector sizes!");

	std::vector<double> prod(len);
	for (size_t i=0; i<len; i++)
		prod[i] = fva[i] * fvb[i];

	return createFloatValue(prod);
}

/// Vector (point-wise) addition
ProtoAtomPtr opencog::plus(const FloatValuePtr& fvpa, const FloatValuePtr& fvpb)
{
	const std::vector<double>& fva = fvpa->value();
	const std::vector<double>& fvb = fvpb->value();
	size_t len = fva.size();
	if (len != fvb.size())
		throw RuntimeException(TRACE_INFO, "Mismatched vector sizes!");

	std::vector<double> sum(len);
	for (size_t i=0; i<len; i++)
		sum[i] = fva[i] + fvb[i];

	return createFloatValue(sum);
}

/// Vector (point-wise) division
ProtoAtomPtr opencog::divide(const FloatValuePtr& fvpa, const FloatValuePtr& fvpb)
{
	const std::vector<double>& fva = fvpa->value();
	const std::vector<double>& fvb = fvpb->value();
	size_t len = fva.size();
	if (len != fvb.size())
		throw RuntimeException(TRACE_INFO, "Mismatched vector sizes!");

	std::vector<double> ratio(len);
	for (size_t i=0; i<len; i++)
		ratio[i] = fva[i] / fvb[i];

	return createFloatValue(ratio);
}
