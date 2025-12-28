/*
 * opencog/atoms/value/Float32Value.h
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

#ifndef _OPENCOG_FLOAT32_VALUE_H
#define _OPENCOG_FLOAT32_VALUE_H

#include <vector>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Float32Values hold an ordered vector of single-precision floats.
 */
class Float32Value
	: public Value
{
protected:
	mutable std::vector<float> _value;

	virtual void update() const {}

	Float32Value(Type t) : Value(t) {}
	Float32Value(Type t, const std::vector<float>& v) : Value(t), _value(v) {}
public:
	Float32Value(const std::vector<float>& v)
		: Value(FLOAT32_VALUE), _value(v) {}
	Float32Value(std::vector<float>&& v)
		: Value(FLOAT32_VALUE), _value(std::move(v)) {}

	virtual ~Float32Value() {}

	const std::vector<float>& value() const { update(); return _value; }
	size_t size() const { return _value.size(); }
	virtual ValuePtr incrementCount(const std::vector<float>&) const;

	/** Returns a string representation of the value. */
	virtual std::string to_string(const std::string& indent = "") const
	{ return to_string(indent, _type); }
	std::string to_string(const std::string& indent, Type) const;

	/** Returns true if two values are equal. */
	virtual bool operator==(const Value&) const;
};

VALUE_PTR_DECL(Float32Value);
CREATE_VALUE_DECL(Float32Value);

// Scalar multiplication and addition
std::vector<float> plus(float, const std::vector<float>&);
std::vector<float> minus(float, const std::vector<float>&);
std::vector<float> minus(const std::vector<float>&, float);
std::vector<float> times(float, const std::vector<float>&);
std::vector<float> divide(float, const std::vector<float>&);

inline
ValuePtr plus(float f, const Float32ValuePtr& fvp) {
	return createFloat32Value(plus(f, fvp->value()));
}
inline
ValuePtr minus(float f, const Float32ValuePtr& fvp) {
	return createFloat32Value(minus(f, fvp->value()));
}
inline
ValuePtr minus(const Float32ValuePtr& fvp, float f) {
	return createFloat32Value(minus(fvp->value(), f));
}
inline
ValuePtr times(float f, const Float32ValuePtr& fvp) {
	return createFloat32Value(times(f, fvp->value()));
}
inline
ValuePtr divide(float f, const Float32ValuePtr& fvp) {
	return createFloat32Value(divide(f, fvp->value()));
}


std::vector<float> plus(const std::vector<float>&, const std::vector<float>&);
std::vector<float> minus(const std::vector<float>&, const std::vector<float>&);
std::vector<float> times(const std::vector<float>&, const std::vector<float>&);
std::vector<float> divide(const std::vector<float>&, const std::vector<float>&);

/// Vector multiplication and addition. When operating on an object
/// times itself, take a sample first; this is needed to correctly
/// handle streaming values, as they issue new values every time
/// they are called. Failing to sample results in violations...
inline
ValuePtr plus(const Float32ValuePtr& fvpa, const Float32ValuePtr& fvpb) {
	if (fvpa != fvpb)
		return createFloat32Value(plus(fvpa->value(), fvpb->value()));
	auto sample = fvpa->value();
	return createFloat32Value(plus(sample, fvpb->value()));
}
inline
ValuePtr minus(const Float32ValuePtr& fvpa, const Float32ValuePtr& fvpb) {
	if (fvpa != fvpb)
		return createFloat32Value(minus(fvpa->value(), fvpb->value()));
	auto sample = fvpa->value();
	return createFloat32Value(minus(sample, fvpb->value()));
}
inline
ValuePtr times(const Float32ValuePtr& fvpa, const Float32ValuePtr& fvpb) {
	if (fvpa != fvpb)
		return createFloat32Value(times(fvpa->value(), fvpb->value()));
	auto sample = fvpa->value();
	return createFloat32Value(times(sample, fvpb->value()));
}
inline
ValuePtr divide(const Float32ValuePtr& fvpa, const Float32ValuePtr& fvpb) {
	if (fvpa != fvpb)
		return createFloat32Value(divide(fvpa->value(), fvpb->value()));
	auto sample = fvpa->value();
	return createFloat32Value(divide(sample, fvpb->value()));
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_FLOAT32_VALUE_H
