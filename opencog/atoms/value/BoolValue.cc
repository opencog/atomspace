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
#include <cstring>
#include <cstdio>

using namespace opencog;

// Bits will be stored in big-endian format, from left to right.
// That is, bit zero is the left-most bit.
static constexpr size_t BITS_PER_WORD = 64;
static size_t word_index(size_t bit_index) {
	return bit_index / BITS_PER_WORD;
}
static size_t bit_offset(size_t bit_index) {
	return BITS_PER_WORD - 1 - bit_index % BITS_PER_WORD;
}
static size_t words_needed(size_t bit_count) {
	return (bit_count + BITS_PER_WORD - 1) / BITS_PER_WORD;
}

// Helper methods for bit manipulation
void BoolValue::set_bit(size_t index, bool value) const
{
	if (index >= _bit_count) return;
	size_t word_idx = word_index(index);
	size_t bit_idx = bit_offset(index);

	if (value) {
		_packed_bits[word_idx] |= (uint64_t(1) << bit_idx);
	} else {
		_packed_bits[word_idx] &= ~(uint64_t(1) << bit_idx);
	}
}

bool BoolValue::get_bit(size_t index) const
{
	if (index >= _bit_count) return false;
	size_t word_idx = word_index(index);
	size_t bit_idx = bit_offset(index);
	return (_packed_bits[word_idx] >> bit_idx) & 1;
}

void BoolValue::pack_vector(const std::vector<bool>& v)
{
	_bit_count = v.size();
	_packed_bits.clear();
	_packed_bits.resize(words_needed(_bit_count), 0);

	for (size_t i = 0; i < _bit_count; i++) {
		if (v[i]) {
			size_t word_idx = word_index(i);
			size_t bit_idx = bit_offset(i);
			_packed_bits[word_idx] |= (uint64_t(1) << bit_idx);
		}
	}
}

std::vector<bool> BoolValue::unpack_vector() const
{
	update();
	std::vector<bool> result(_bit_count);
	for (size_t i = 0; i < _bit_count; i++) {
		result[i] = get_bit(i);
	}
	return result;
}

// Constructors
BoolValue::BoolValue(bool v) : Value(BOOL_VALUE), _bit_count(1)
{
	_packed_bits.resize(1, 0);
	if (v) _packed_bits[0] = 1;
}

BoolValue::BoolValue(const std::vector<bool>& v) : Value(BOOL_VALUE)
{
	pack_vector(v);
}

BoolValue::BoolValue(Type t, const std::vector<bool>& v) : Value(t)
{
	pack_vector(v);
}

std::vector<bool> BoolValue::value() const
{
	update();
	return unpack_vector();
}

std::string BoolValue::to_string(const std::string& indent) const
{
	return to_string(indent, _type);
}

ValuePtr BoolValue::value_at_index(size_t idx) const
{
	bool b = get_bit(idx);
	return createBoolValue(b);
}

bool BoolValue::operator==(const Value& other) const
{
	if (BOOL_VALUE != other.get_type()) return false;

	const BoolValue* bov = (const BoolValue*) &other;

	if (_bit_count != bov->_bit_count) return false;

	// Compare packed bits
	size_t word_count = words_needed(_bit_count);
	for (size_t i = 0; i < word_count - 1; i++) {
		if (_packed_bits[i] != bov->_packed_bits[i])
			return false;
	}

	// For the last word, only compare the relevant bits
	if (word_count > 0) {
		size_t last_bits = bit_offset(_bit_count);
		if (last_bits == 0) last_bits = BITS_PER_WORD;
		uint64_t mask = (uint64_t(1) << last_bits) - 1;
		if ((_packed_bits[word_count - 1] & mask) != (bov->_packed_bits[word_count - 1] & mask))
			return false;
	}

	return true;
}

// ==============================================================

std::string BoolValue::to_string(const std::string& indent, Type t) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(t);
	SAFE_UPDATE(rv,
	{
		if (_bit_count < 16)
		{
			// For short bitstrings, print individual bits
			for (size_t i = 0; i < _bit_count; i++)
			{
				if (get_bit(i)) rv += " 1";
				else rv += " 0";
			}
			rv += ")";
			return rv;
		}

		// For longer bitstrings, print in hexadecimal
		rv += " 0x";

		// The native storage format used above is big endian, in that
		// bit zero is the left-most bit, bit one is to the right of that,
		// and so on. This is easier to print than little-endian (which
		// would force us to bit swap) but still presents some challenge.
		size_t word_count = words_needed(_bit_count);

		// Print full 16 hex digits if we're lucky.
		int bit_align = _bit_count % 64;
		if (0 == bit_align)
		{
			for (size_t w = 0; w < word_count; w++) {
				uint64_t word = _packed_bits[w];
				char buf[17];
				snprintf(buf, sizeof(buf), "%016lx", word);
				rv += buf;
			}
			rv += ")";
			return rv;
		}

		// First word to be padded by zeros, as needed.
		uint64_t word = _packed_bits[0];
		uint64_t mask = (1ULL << bit_align) - 1;
		uint64_t carry = (word & mask) << (64 - bit_align);
		word >>= bit_align;
		int width = (65 - bit_align) >> 2;
		char buf[17];
		snprintf(buf, sizeof(buf), "%0*lx", width, word);
		rv += buf;

		// Middle words are spliced from low bits of previous
		// and high bits of current.
		for (size_t w = 1; w < word_count - 1; w++)
		{
			uint64_t word = _packed_bits[w];
			uint64_t mask = (1ULL << bit_align) - 1;
			uint64_t rbits = (word & mask) << (64 - bit_align);
			word >>= bit_align;
			word = word | carry;
			carry = rbits;
			snprintf(buf, sizeof(buf), "%016lx", word);
			rv += buf;
		}

		// Last word handling depends on how many carry bits
		// we have to make room for.
		word = _packed_bits[word_count - 1];
		if (32 >= bit_align)
		{
			word >>= bit_align;
			word = word | carry;
			word >>= (64 - 2* bit_align);
			snprintf(buf, sizeof(buf), "%lx", word);
			rv += buf;
		}
		else
		{
			uint64_t rbits = (word & mask);
			word >>= bit_align;
			word = word | carry;
			snprintf(buf, sizeof(buf), "%016lx", word);
			rv += buf;
			snprintf(buf, sizeof(buf), "%lx", rbits);
			rv += buf;
		}
	});

	rv += ")";
	return rv;
}

// ==============================================================
// Optimized packed boolean operations (static/internal use only)

static void bool_and_packed(std::vector<uint64_t>& result, size_t& result_bits,
                            bool scalar, const std::vector<uint64_t>& packed, size_t bits)
{
	result_bits = bits;
	size_t word_count = words_needed(bits);
	result.resize(word_count);

	if (scalar) {
		// Copy all bits if scalar is true
		for (size_t i = 0; i < word_count; i++) {
			result[i] = packed[i];
		}
	} else {
		// All bits are false if scalar is false
		for (size_t i = 0; i < word_count; i++) {
			result[i] = 0;
		}
	}

	// Clean up unused bits in the last word
	if (bits > 0 && word_count > 0) {
		size_t last_bits = bit_offset(bits);
		if (last_bits > 0) {
			uint64_t mask = (uint64_t(1) << last_bits) - 1;
			result[word_count - 1] &= mask;
		}
	}
}

static void bool_or_packed(std::vector<uint64_t>& result, size_t& result_bits,
                           bool scalar, const std::vector<uint64_t>& packed, size_t bits)
{
	result_bits = bits;
	size_t word_count = words_needed(bits);
	result.resize(word_count);

	if (scalar) {
		// All bits are true if scalar is true
		for (size_t i = 0; i < word_count; i++) {
			result[i] = ~uint64_t(0);
		}
		// Clean up the last word
		if (bits > 0 && word_count > 0) {
			size_t last_bits = bit_offset(bits);
			if (last_bits > 0) {
				uint64_t mask = (uint64_t(1) << last_bits) - 1;
				result[word_count - 1] &= mask;
			}
		}
	} else {
		// Copy all bits if scalar is false
		for (size_t i = 0; i < word_count; i++) {
			result[i] = packed[i];
		}
	}
}

static void bool_not_packed(std::vector<uint64_t>& result, size_t& result_bits,
                            const std::vector<uint64_t>& packed, size_t bits)
{
	result_bits = bits;
	size_t word_count = words_needed(bits);
	result.resize(word_count);

	// Invert all words
	for (size_t i = 0; i < word_count; i++) {
		result[i] = ~packed[i];
	}

	// Clean up unused bits in the last word
	if (bits > 0 && word_count > 0) {
		size_t last_bits = bit_offset(bits);
		if (last_bits > 0) {
			uint64_t mask = (uint64_t(1) << last_bits) - 1;
			result[word_count - 1] &= mask;
		}
	}
}

static void bool_and_packed(std::vector<uint64_t>& result, size_t& result_bits,
                            const std::vector<uint64_t>& packed_a, size_t bits_a,
                            const std::vector<uint64_t>& packed_b, size_t bits_b)
{
	// Handle scalar cases
	if (bits_a == 1) {
		bool scalar = packed_a[0] & 1;
		bool_and_packed(result, result_bits, scalar, packed_b, bits_b);
		return;
	}
	if (bits_b == 1) {
		bool scalar = packed_b[0] & 1;
		bool_and_packed(result, result_bits, scalar, packed_a, bits_a);
		return;
	}

	// Vector AND
	result_bits = std::max(bits_a, bits_b);
	size_t word_count = words_needed(result_bits);
	size_t word_count_a = words_needed(bits_a);
	size_t word_count_b = words_needed(bits_b);
	result.resize(word_count, 0);

	size_t min_words = std::min(word_count_a, word_count_b);

	// AND the common words
	for (size_t i = 0; i < min_words; i++) {
		result[i] = packed_a[i] & packed_b[i];
	}

	// The rest are implicitly false (already zeroed)
}

static void bool_or_packed(std::vector<uint64_t>& result, size_t& result_bits,
                           const std::vector<uint64_t>& packed_a, size_t bits_a,
                           const std::vector<uint64_t>& packed_b, size_t bits_b)
{
	// Handle scalar cases
	if (bits_a == 1) {
		bool scalar = packed_a[0] & 1;
		bool_or_packed(result, result_bits, scalar, packed_b, bits_b);
		return;
	}
	if (bits_b == 1) {
		bool scalar = packed_b[0] & 1;
		bool_or_packed(result, result_bits, scalar, packed_a, bits_a);
		return;
	}

	// Vector OR
	result_bits = std::max(bits_a, bits_b);
	size_t word_count = words_needed(result_bits);
	size_t word_count_a = words_needed(bits_a);
	size_t word_count_b = words_needed(bits_b);
	result.resize(word_count, 0);

	size_t min_words = std::min(word_count_a, word_count_b);

	// OR the common words
	for (size_t i = 0; i < min_words; i++) {
		result[i] = packed_a[i] | packed_b[i];
	}

	// Copy remaining words from the longer vector
	if (word_count_a > word_count_b) {
		for (size_t i = min_words; i < word_count_a; i++) {
			result[i] = packed_a[i];
		}
	} else if (word_count_b > word_count_a) {
		for (size_t i = min_words; i < word_count_b; i++) {
			result[i] = packed_b[i];
		}
	}

	// Clean up the last word if needed
	if (result_bits > 0 && word_count > 0) {
		size_t last_bits = bit_offset(result_bits);
		if (last_bits > 0) {
			uint64_t mask = (uint64_t(1) << last_bits) - 1;
			result[word_count - 1] &= mask;
		}
	}
}

// Boolean operation implementations that work directly with BoolValuePtr

ValuePtr opencog::bool_and(bool scalar, const BoolValuePtr& fvp)
{
	std::vector<uint64_t> result;
	size_t result_bits;
	bool_and_packed(result, result_bits, scalar, fvp->get_packed_bits(), fvp->get_bit_count());

	auto rv = createBoolValue(false);
	rv->set_packed_data(std::move(result), result_bits);
	return rv;
}

ValuePtr opencog::bool_or(bool scalar, const BoolValuePtr& fvp)
{
	std::vector<uint64_t> result;
	size_t result_bits;
	bool_or_packed(result, result_bits, scalar, fvp->get_packed_bits(), fvp->get_bit_count());

	auto rv = createBoolValue(false);
	rv->set_packed_data(std::move(result), result_bits);
	return rv;
}

ValuePtr opencog::bool_not(const BoolValuePtr& fvp)
{
	std::vector<uint64_t> result;
	size_t result_bits;
	bool_not_packed(result, result_bits, fvp->get_packed_bits(), fvp->get_bit_count());

	auto rv = createBoolValue(false);
	rv->set_packed_data(std::move(result), result_bits);
	return rv;
}

ValuePtr opencog::bool_and(const BoolValuePtr& fvpa, const BoolValuePtr& fvpb)
{
	// Handle streaming values by taking a sample
	if (fvpa == fvpb) {
		// Sample the value first
		auto sample = fvpa->value();
		BoolValue temp_sample(sample);

		std::vector<uint64_t> result;
		size_t result_bits;
		bool_and_packed(result, result_bits, temp_sample.get_packed_bits(), temp_sample.get_bit_count(),
		                fvpb->get_packed_bits(), fvpb->get_bit_count());

		auto rv = createBoolValue(false);
		rv->set_packed_data(std::move(result), result_bits);
		return rv;
	}

	std::vector<uint64_t> result;
	size_t result_bits;
	bool_and_packed(result, result_bits, fvpa->get_packed_bits(), fvpa->get_bit_count(),
	                fvpb->get_packed_bits(), fvpb->get_bit_count());

	auto rv = createBoolValue(false);
	rv->set_packed_data(std::move(result), result_bits);
	return rv;
}

ValuePtr opencog::bool_or(const BoolValuePtr& fvpa, const BoolValuePtr& fvpb)
{
	// Handle streaming values by taking a sample
	if (fvpa == fvpb) {
		// Sample the value first
		auto sample = fvpa->value();
		BoolValue temp_sample(sample);

		std::vector<uint64_t> result;
		size_t result_bits;
		bool_or_packed(result, result_bits, temp_sample.get_packed_bits(), temp_sample.get_bit_count(),
		               fvpb->get_packed_bits(), fvpb->get_bit_count());

		auto rv = createBoolValue(false);
		rv->set_packed_data(std::move(result), result_bits);
		return rv;
	}

	std::vector<uint64_t> result;
	size_t result_bits;
	bool_or_packed(result, result_bits, fvpa->get_packed_bits(), fvpa->get_bit_count(),
	               fvpb->get_packed_bits(), fvpb->get_bit_count());

	auto rv = createBoolValue(false);
	rv->set_packed_data(std::move(result), result_bits);
	return rv;
}

// Adds factory when the library is loaded.
DEFINE_VALUE_FACTORY(BOOL_VALUE,
                     createBoolValue, std::vector<bool>)
DEFINE_VALUE_FACTORY(BOOL_VALUE,
                     createBoolValue, bool)
