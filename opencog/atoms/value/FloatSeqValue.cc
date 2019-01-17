/*
 * opencog/atoms/value/FloatSeqValue.cc
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
#include <opencog/atoms/value/FloatSeqValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

bool FloatSeqValue::operator==(const Value& other) const
{
	if (FLOAT_SEQ_VALUE != other.get_type()) return false;

   const FloatSeqValue* fov = (const FloatSeqValue*) &other;

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

std::string FloatSeqValue::to_string(const std::string& indent) const
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
ValuePtr opencog::times(double scalar, const FloatSeqValuePtr& fvp)
{
	const std::vector<double>& fv = fvp->value();
	size_t len = fv.size();
	std::vector<double> prod(len);
	for (size_t i=0; i<len; i++)
		prod[i] = scalar * fv[i];

	return createFloatSeqValue(prod);
}

/// Scalar addition
ValuePtr opencog::plus(double scalar, const FloatSeqValuePtr& fvp)
{
	const std::vector<double>& fv = fvp->value();
	size_t len = fv.size();
	std::vector<double> sum(len);
	for (size_t i=0; i<len; i++)
		sum[i] = scalar + fv[i];

	return createFloatSeqValue(sum);
}

/// Scalar division
ValuePtr opencog::divide(double scalar, const FloatSeqValuePtr& fvp)
{
	const std::vector<double>& fv = fvp->value();
	size_t len = fv.size();
	std::vector<double> ratio(len);
	for (size_t i=0; i<len; i++)
		ratio[i] = scalar / fv[i];

	return createFloatSeqValue(ratio);
}

/// Vector (point-wise) multiplication
ValuePtr opencog::times(const FloatSeqValuePtr& fvpa, const FloatSeqValuePtr& fvpb)
{
	const std::vector<double>& fva = fvpa->value();
	const std::vector<double>& fvb = fvpb->value();
	size_t len = fva.size();
	if (len != fvb.size())
		throw RuntimeException(TRACE_INFO, "Mismatched vector sizes!");

	std::vector<double> prod(len);
	for (size_t i=0; i<len; i++)
		prod[i] = fva[i] * fvb[i];

	return createFloatSeqValue(prod);
}

/// Vector (point-wise) addition
ValuePtr opencog::plus(const FloatSeqValuePtr& fvpa, const FloatSeqValuePtr& fvpb)
{
	const std::vector<double>& fva = fvpa->value();
	const std::vector<double>& fvb = fvpb->value();
	size_t len = fva.size();
	if (len != fvb.size())
		throw RuntimeException(TRACE_INFO, "Mismatched vector sizes!");

	std::vector<double> sum(len);
	for (size_t i=0; i<len; i++)
		sum[i] = fva[i] + fvb[i];

	return createFloatSeqValue(sum);
}

/// Vector (point-wise) division
ValuePtr opencog::divide(const FloatSeqValuePtr& fvpa, const FloatSeqValuePtr& fvpb)
{
	const std::vector<double>& fva = fvpa->value();
	const std::vector<double>& fvb = fvpb->value();
	size_t len = fva.size();
	if (len != fvb.size())
		throw RuntimeException(TRACE_INFO, "Mismatched vector sizes!");

	std::vector<double> ratio(len);
	for (size_t i=0; i<len; i++)
		ratio[i] = fva[i] / fvb[i];

	return createFloatSeqValue(ratio);
}

// Adds factory when the library is loaded.
DEFINE_VALUE_FACTORY(FLOAT_SEQ_VALUE,
                     createFloatSeqValue, std::vector<double>)
DEFINE_VALUE_FACTORY(FLOAT_SEQ_VALUE,
                     createFloatSeqValue, double)
