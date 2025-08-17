/*
 * opencog/atoms/value/BoolValue.h
 *
 * Copyright (C) 2015,2022 Linas Vepstas
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

#ifndef _OPENCOG_BOOL_VALUE_H
#define _OPENCOG_BOOL_VALUE_H

#include <vector>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * BoolValues hold an ordered vector of bools.
 *
 * This provides a reference interface for bools. Internally, bits
 * are packed into uint64_t values for efficient boolean operations.
 * Boolean-and, boolean-or, and boolean-not operate on 64-bit chunks
 * for improved performance.
 */
class BoolValue;
typedef std::shared_ptr<const BoolValue> BoolValuePtr;

class BoolValue
	: public Value
{
protected:
	mutable std::vector<uint64_t> _packed_bits;
	mutable size_t _bit_count;
	mutable std::vector<bool> _value_cache;  // Cache for Python bindings compatibility

	virtual void update() const {}

	BoolValue(Type t) : Value(t), _bit_count(0) {}

	void set_bit(size_t index, bool value) const;
	bool get_bit(size_t index) const;
	void pack_vector(const std::vector<bool>& v);
	std::vector<bool> unpack_vector() const;

public:
	// Helper methods for bit manipulation
	static constexpr size_t BITS_PER_WORD = 64;

	static size_t word_index(size_t bit_index) {
		return bit_index / BITS_PER_WORD;
	}

	static size_t bit_offset(size_t bit_index) {
		return bit_index % BITS_PER_WORD;
	}

	static size_t words_needed(size_t bit_count) {
		return (bit_count + BITS_PER_WORD - 1) / BITS_PER_WORD;
	}

	BoolValue(bool v);
	BoolValue(const std::vector<bool>& v);
	BoolValue(unsigned long);
	BoolValue(Type t, const std::vector<bool>& v);

	virtual ~BoolValue() {}

	const std::vector<bool>& value() const;  // Returns cached value for Python bindings
	size_t size() const { return _bit_count; }
	ValuePtr value_at_index(size_t) const;

	/** Returns a string representation of the value. */
	virtual std::string to_string(const std::string& indent = "") const;
	std::string to_string(const std::string& indent, Type) const;

	/** Returns true if two values are equal. */
	virtual bool operator==(const Value&) const;

	// Public methods to get packed data (for bool operations)
	const std::vector<uint64_t>& get_packed_bits() const { return _packed_bits; }
	size_t get_bit_count() const { return _bit_count; }
	void set_packed_data(std::vector<uint64_t>&& bits, size_t count) {
		_packed_bits = std::move(bits);
		_bit_count = count;
	}
};

static inline BoolValuePtr BoolValueCast(const ValuePtr& a)
	{ return std::dynamic_pointer_cast<const BoolValue>(a); }

static inline const ValuePtr ValueCast(const BoolValuePtr& fv)
{
	return std::shared_ptr<Value>(fv, (Value*) fv.get());
}

template<typename ... Type>
static inline std::shared_ptr<BoolValue> createBoolValue(Type&&... args) {
	return std::make_shared<BoolValue>(std::forward<Type>(args)...);
}

// Boolean operation functions that work directly with BoolValuePtr
ValuePtr bool_and(bool f, const BoolValuePtr& fvp);
ValuePtr bool_or(bool f, const BoolValuePtr& fvp);
ValuePtr bool_not(const BoolValuePtr& fvp);

// Vector boolean operations
ValuePtr bool_and(const BoolValuePtr& fvpa, const BoolValuePtr& fvpb);
ValuePtr bool_or(const BoolValuePtr& fvpa, const BoolValuePtr& fvpb);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_BOOL_VALUE_H
