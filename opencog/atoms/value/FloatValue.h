/*
 * opencog/atoms/value/FloatValue.h
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

#ifndef _OPENCOG_FLOAT_VALUE_H
#define _OPENCOG_FLOAT_VALUE_H

#include <vector>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * FloatValues hold an ordered vector of doubles.
 */
class FloatValue
	: public Value
{
protected:
	mutable std::vector<double> _value;

	virtual void update() const {}

	FloatValue(Type t) : Value(t) {}
public: // XXX should be protected...
	FloatValue(Type t, const std::vector<double>& v) : Value(t), _value(v) {}

public:
	FloatValue(double v) : Value(FLOAT_VALUE) { _value.push_back(v); }
	FloatValue(const std::vector<double>& v)
		: Value(FLOAT_VALUE), _value(v) {}

	virtual ~FloatValue() {}

	const std::vector<double>& value() const { update(); return _value; }

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent = "") const;

	/** Returns true if two atoms are equal.  */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<const FloatValue> FloatValuePtr;
static inline FloatValuePtr FloatValueCast(const ValuePtr& a)
	{ return std::dynamic_pointer_cast<const FloatValue>(a); }

static inline const ValuePtr ValueCast(const FloatValuePtr& fv)
{
	return std::shared_ptr<Value>(fv, (Value*) fv.get());
}

template<typename ... Type>
static inline std::shared_ptr<FloatValue> createFloatValue(Type&&... args) {
	return std::make_shared<FloatValue>(std::forward<Type>(args)...);
}

// Scalar multiplication and addition
ValuePtr times(double, const FloatValuePtr&);
ValuePtr plus(double, const FloatValuePtr&);
ValuePtr divide(double, const FloatValuePtr&);

// Vector multiplication and addition
ValuePtr times(const FloatValuePtr&, const FloatValuePtr&);
ValuePtr plus(const FloatValuePtr&, const FloatValuePtr&);
ValuePtr divide(const FloatValuePtr&, const FloatValuePtr&);


/** @}*/
} // namespace opencog

#endif // _OPENCOG_FLOAT_VALUE_H
