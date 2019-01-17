/*
 * opencog/atoms/value/FloatSeqValue.h
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

#ifndef _OPENCOG_FLOAT_SEQ_VALUE_H
#define _OPENCOG_FLOAT_SEQ_VALUE_H

#include <vector>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * FloatSeqValues hold an ordered vector of doubles.
 */
class FloatSeqValue
	: public Value
{
protected:
	mutable std::vector<double> _value;

	virtual void update() const {}

	FloatSeqValue(Type t) : Value(t) {}
public: // XXX should be protected...
	FloatSeqValue(Type t, const std::vector<double>& v) : Value(t), _value(v) {}

public:
	FloatSeqValue(double v) : Value(FLOAT_SEQ_VALUE) { _value.push_back(v); }
	FloatSeqValue(const std::vector<double>& v)
		: Value(FLOAT_SEQ_VALUE), _value(v) {}

	virtual ~FloatSeqValue() {}

	const std::vector<double>& value() const { update(); return _value; }

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent = "") const;

	/** Returns true if two atoms are equal.  */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<const FloatSeqValue> FloatSeqValuePtr;
static inline FloatSeqValuePtr FloatSeqValueCast(const ValuePtr& a)
	{ return std::dynamic_pointer_cast<const FloatSeqValue>(a); }

template<typename ... Type>
static inline std::shared_ptr<FloatSeqValue> createFloatSeqValue(Type&&... args) {
	return std::make_shared<FloatSeqValue>(std::forward<Type>(args)...);
}

// Scalar multiplication and addition
ValuePtr times(double, const FloatSeqValuePtr&);
ValuePtr plus(double, const FloatSeqValuePtr&);
ValuePtr divide(double, const FloatSeqValuePtr&);

// Vector multiplication and addition
ValuePtr times(const FloatSeqValuePtr&, const FloatSeqValuePtr&);
ValuePtr plus(const FloatSeqValuePtr&, const FloatSeqValuePtr&);
ValuePtr divide(const FloatSeqValuePtr&, const FloatSeqValuePtr&);


/** @}*/
} // namespace opencog

#endif // _OPENCOG_FLOAT_SEQ_VALUE_H
