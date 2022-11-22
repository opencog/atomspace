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
 */
class BoolValue
	: public Value
{
protected:
	mutable std::vector<bool> _value;

	virtual void update() const {}

	BoolValue(Type t) : Value(t) {}

public:
	BoolValue(bool v) : Value(BOOL_VALUE) { _value.push_back(v); }
	BoolValue(const std::vector<bool>& v)
		: Value(BOOL_VALUE), _value(v) {}
	BoolValue(unsigned long);
	BoolValue(Type t, const std::vector<bool>& v) : Value(t), _value(v) {}

	virtual ~BoolValue() {}

	const std::vector<bool>& value() const { update(); return _value; }
	size_t size() const { return _value.size(); }

	/** Returns a string representation of the value. */
	virtual std::string to_string(const std::string& indent = "") const
	{ return to_string(indent, _type); }
	std::string to_string(const std::string& indent, Type) const;

	/** Returns true if two values are equal. */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<const BoolValue> BoolValuePtr;
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

// Scalar boolean ops
std::vector<bool> bool_and(bool, const std::vector<bool>&);
std::vector<bool> bool_or(bool, const std::vector<bool>&);
std::vector<bool> bool_not(const std::vector<bool>&);

inline
ValuePtr bool_and(bool f, const BoolValuePtr& fvp) {
	return createBoolValue(bool_and(f, fvp->value()));
}
inline
ValuePtr bool_or(bool f, const BoolValuePtr& fvp) {
	return createBoolValue(bool_or(f, fvp->value()));
}
inline
ValuePtr bool_not(const BoolValuePtr& fvp) {
	return createBoolValue(bool_not(fvp->value()));
}

std::vector<bool> bool_and(const std::vector<bool>&, const std::vector<bool>&);
std::vector<bool> bool_or(const std::vector<bool>&, const std::vector<bool>&);

/// Vector boolean ops. When operating on an object bool_op and itself,
/// take a sample first; this is needed to correctly handle streaming
/// values, as they issue new values every time they are called. Failing
/// to sample results in violations...
inline
ValuePtr bool_and(const BoolValuePtr& fvpa, const BoolValuePtr& fvpb) {
	if (fvpa != fvpb)
		return createBoolValue(bool_and(fvpa->value(), fvpb->value()));
	auto sample = fvpa->value();
	return createBoolValue(bool_and(sample, fvpb->value()));
}
inline
ValuePtr bool_or(const BoolValuePtr& fvpa, const BoolValuePtr& fvpb) {
	if (fvpa != fvpb)
		return createBoolValue(bool_or(fvpa->value(), fvpb->value()));
	auto sample = fvpa->value();
	return createBoolValue(bool_or(sample, fvpb->value()));
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_BOOL_VALUE_H
