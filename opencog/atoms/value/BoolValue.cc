/*
 * opencog/atoms/value/BoolValue.cc
 *
 * Copyright (C) 2015, 2022 Linas Vepstas
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
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

BoolValue::BoolValue(unsigned long mask)
	: Value(BOOL_VALUE)
{
	// Avoid padding with zeroes.
	unsigned long mcopy = mask;
	size_t maxlen = 1;
	for (size_t i=0; i<8*sizeof(unsigned long); i++)
	{
		if (mcopy & 0x1) maxlen = i;
		mcopy >>= 1;
	}

	// We print vectors as little-endians but the integer
	// itself is a big-endian, so we reverse the bit pattern.
	_value.resize(maxlen+1);
	for (size_t i=0; i<=maxlen; i++)
	{
		_value[maxlen-i] = (mask & 0x1);
		mask >>= 1;
	}
}

ValuePtr BoolValue::value_at_index(size_t idx) const
{
	bool b = false;
	if (_value.size() > idx) b = _value[idx];
	return createBoolValue(b);
}

bool BoolValue::operator==(const Value& other) const
{
	if (BOOL_VALUE != other.get_type()) return false;

   const BoolValue* bov = (const BoolValue*) &other;

	if (_value.size() != bov->_value.size()) return false;
	size_t len = _value.size();
	for (size_t i=0; i<len; i++)
		if (_value[i] != bov->_value[i])
			return false;
	return true;
}

// ==============================================================

std::string BoolValue::to_string(const std::string& indent, Type t) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(t);
	SAFE_UPDATE(rv,
	{
		for (bool v :_value)
		{
			if (v) rv += " 1";
			else rv += " 0";
		}
	});

	rv += ")";
	return rv;
}

// ==============================================================

/// Scalar multiplication
std::vector<bool> opencog::bool_and(bool scalar, const std::vector<bool>& bv)
{
	if (scalar)
	{
		std::vector<bool> prod(bv);
		return prod;
	}
	else
	{
		size_t len = bv.size();
		std::vector<bool> prod(len, false);
		return prod;
	}
}

/// Scalar addition
std::vector<bool> opencog::bool_or(bool scalar, const std::vector<bool>& bv)
{
	if (scalar)
	{
		size_t len = bv.size();
		std::vector<bool> sum(len, true);
		return sum;
	}
	else
	{
		std::vector<bool> sum(bv);
		return sum;
	}
}

/// Inversion
std::vector<bool> opencog::bool_not(const std::vector<bool>& bv)
{
	size_t len = bv.size();
	std::vector<bool> inv(len);
	for (size_t i=0; i<len; i++)
		inv[i] = not bv[i];

	return inv;
}

/// Vector (point-wise) multiplication
/// The shorter vector is assumed to be false-padded.
std::vector<bool> opencog::bool_and(const std::vector<bool>& bva,
                                  const std::vector<bool>& bvb)
{
	size_t lena = bva.size();
	size_t lenb = bvb.size();

	if (1 == lena)
		return bool_and(bva[0], bvb);

	if (1 == lenb)
		return bool_and(bvb[0], bva);

	std::vector<bool> prod(std::max(lena, lenb));
	if (lena < lenb)
	{
		size_t i=0;
		for (; i<lena; i++)
			prod[i] = bva[i] and bvb[i];
		for (; i<lenb; i++)
			prod[i] = false;
	}
	else
	{
		size_t i=0;
		for (; i<lenb; i++)
			prod[i] = bva[i] and bvb[i];
		for (; i<lena; i++)
			prod[i] = false;
	}
	return prod;
}

/// Vector (point-wise) addition
/// The shorter vector is assumed to be true-padded.
/// Unless the shorter vector is a scalar, in which case we do
/// scalar addition. This is the "right thing to do", because that
/// is the general user intent.  We could detect this case in all
/// the callers to this routine, or we could just handle it here.
/// This may seem messy to you, but this is the easiest solution.
std::vector<bool> opencog::bool_or(const std::vector<bool>& bva,
                                   const std::vector<bool>& bvb)
{
	size_t lena = bva.size();
	size_t lenb = bvb.size();

	std::vector<bool> sum(std::max(lena, lenb));
	if (1 == lena)
	{
		return opencog::bool_or(bva[0], bvb);
	}
	else
	if (1 == lenb)
	{
		return opencog::bool_or(bvb[0], bva);
	}
	else
	if (lena < lenb)
	{
		size_t i=0;
		for (; i<lena; i++)
			sum[i] = bva[i] or bvb[i];
		for (; i<lenb; i++)
			sum[i] = bvb[i];
	}
	else
	{
		size_t i=0;
		for (; i<lenb; i++)
			sum[i] = bva[i] or bvb[i];
		for (; i<lena; i++)
			sum[i] = bva[i];
	}
	return sum;
}

// Adds factory when the library is loaded.
DEFINE_VALUE_FACTORY(BOOL_VALUE,
                     createBoolValue, std::vector<bool>)
DEFINE_VALUE_FACTORY(BOOL_VALUE,
                     createBoolValue, bool)
DEFINE_VALUE_FACTORY(BOOL_VALUE,
                     createBoolValue, unsigned long)
