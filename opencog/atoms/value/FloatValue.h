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
	friend class TransposeColumn;

protected:
	mutable std::vector<double> _value;

	virtual void update() const {}
	std::string to_string(const std::string&, Type) const;

	FloatValue(Type t) : Value(t) {}
	FloatValue(Type t, const std::vector<double>& v) : Value(t), _value(v) {}
public:
	FloatValue(double v) : Value(FLOAT_VALUE) { _value.push_back(v); }
	FloatValue(const std::vector<double>& v)
		: Value(FLOAT_VALUE), _value(v) {}
	FloatValue(std::vector<double>&& v)
		: Value(FLOAT_VALUE), _value(std::move(v)) {}

	virtual ~FloatValue() {}

	const std::vector<double>& value() const { update(); return _value; }
	size_t size() const { return _value.size(); }
	virtual ValuePtr incrementCount(const std::vector<double>&) const;
	virtual ValuePtr incrementCount(size_t, double) const;

	/** Returns a string representation of the value. */
	virtual std::string to_string(const std::string& indent = "") const
	{ return to_string(indent, _type); }

	/** Returns true if two values are equal. */
	virtual bool operator==(const Value&) const;

	/** Optimized less-than comparison for FloatValue.
	 * Compares by type name first, then vector length, then individual floats. */
	virtual bool operator<(const Value& other) const;
};

VALUE_PTR_DECL(FloatValue);
CREATE_VALUE_DECL(FloatValue);

// Scalar multiplication and addition
std::vector<double> plus(double, const std::vector<double>&);
std::vector<double> minus(double, const std::vector<double>&);
std::vector<double> minus(const std::vector<double>&, double);
std::vector<double> times(double, const std::vector<double>&);
std::vector<double> divide(double, const std::vector<double>&);

inline
ValuePtr plus(double f, const FloatValuePtr& fvp) {
	return createFloatValue(plus(f, fvp->value()));
}
inline
ValuePtr minus(double f, const FloatValuePtr& fvp) {
	return createFloatValue(minus(f, fvp->value()));
}
inline
ValuePtr minus(const FloatValuePtr& fvp, double f) {
	return createFloatValue(minus(fvp->value(), f));
}
inline
ValuePtr times(double f, const FloatValuePtr& fvp) {
	return createFloatValue(times(f, fvp->value()));
}
inline
ValuePtr divide(double f, const FloatValuePtr& fvp) {
	return createFloatValue(divide(f, fvp->value()));
}


std::vector<double> plus(const std::vector<double>&, const std::vector<double>&);
std::vector<double> minus(const std::vector<double>&, const std::vector<double>&);
std::vector<double> times(const std::vector<double>&, const std::vector<double>&);
std::vector<double> divide(const std::vector<double>&, const std::vector<double>&);

/// Vector multiplication and addition. When operating on an object
/// times itself, take a sample first; this is needed to correctly
/// handle streaming values, as they issue new values every time
/// they are called. Failing to sample results in violations...
inline
ValuePtr plus(const FloatValuePtr& fvpa, const FloatValuePtr& fvpb) {
	if (fvpa != fvpb)
		return createFloatValue(plus(fvpa->value(), fvpb->value()));
	auto sample = fvpa->value();
	return createFloatValue(plus(sample, fvpb->value()));
}
inline
ValuePtr minus(const FloatValuePtr& fvpa, const FloatValuePtr& fvpb) {
	if (fvpa != fvpb)
		return createFloatValue(minus(fvpa->value(), fvpb->value()));
	auto sample = fvpa->value();
	return createFloatValue(minus(sample, fvpb->value()));
}
inline
ValuePtr times(const FloatValuePtr& fvpa, const FloatValuePtr& fvpb) {
	if (fvpa != fvpb)
		return createFloatValue(times(fvpa->value(), fvpb->value()));
	auto sample = fvpa->value();
	return createFloatValue(times(sample, fvpb->value()));
}
inline
ValuePtr divide(const FloatValuePtr& fvpa, const FloatValuePtr& fvpb) {
	if (fvpa != fvpb)
		return createFloatValue(divide(fvpa->value(), fvpb->value()));
	auto sample = fvpa->value();
	return createFloatValue(divide(sample, fvpb->value()));
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_FLOAT_VALUE_H
