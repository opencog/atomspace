/*
 * opencog/opencl/stream/OpenclFloatValue.h
 *
 * Copyright (C) 2025 Linas Vepstas
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

#ifndef _OPENCOG_OPENCL_FLOAT_VALUE_H
#define _OPENCOG_OPENCL_FLOAT_VALUE_H

#include <vector>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/opencl/opencl-types/opencl_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * OpenclFloatValues hold an ordered vector of doubles.
 */
class OpenclFloatValue
	: public FloatValue
{
protected:
	virtual void update() const {}

	OpenclFloatValue(Type t) : Value(t) {}
public:
	OpenclFloatValue(double v) : Value(OPENCL_FLOAT_VALUE) { _value.push_back(v); }
	OpenclFloatValue(const std::vector<double>& v)
		: Value(OPENCL_FLOAT_VALUE), _value(v) {}
	OpenclFloatValue(Type t, const std::vector<double>& v) : Value(t), _value(v) {}

	virtual ~OpenclFloatValue() {}

	const std::vector<double>& value() const { update(); return _value; }
	size_t size() const { return _value.size(); }

	/** Returns true if two values are equal. */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<const OpenclFloatValue> OpenclFloatValuePtr;
static inline OpenclFloatValuePtr OpenclFloatValueCast(const ValuePtr& a)
	{ return std::dynamic_pointer_cast<const OpenclFloatValue>(a); }

static inline const ValuePtr ValueCast(const OpenclFloatValuePtr& fv)
{
	return std::shared_ptr<Value>(fv, (Value*) fv.get());
}

template<typename ... Type>
static inline std::shared_ptr<OpenclFloatValue> createOpenclFloatValue(Type&&... args) {
	return std::make_shared<OpenclFloatValue>(std::forward<Type>(args)...);
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_OPENCL_FLOAT_VALUE_H
