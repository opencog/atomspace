/*
 * opencog/atoms/value/StringValue.h
 *
 * Copyright (C) 2015, 2016 Linas Vepstas
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

#ifndef _OPENCOG_STRING_VALUE_H
#define _OPENCOG_STRING_VALUE_H

#include <string>
#include <vector>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * StringValues hold an ordered vector of std::strings.
 */
class StringValue
	: public Value
{
protected:
	std::vector<std::string> _value;

public:
	StringValue(const std::string& v)
		: Value(STRING_VALUE) { _value.push_back(v); }
	StringValue(const std::vector<std::string>& v)
		: Value(STRING_VALUE), _value(v) {}

	virtual ~StringValue() {}

	const std::vector<std::string>& value() const { return _value; }
	size_t size() const {return _value.size(); }

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent = "") const;

	/** Returns true if the two atoms are equal.  */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<const StringValue> StringValuePtr;
static inline StringValuePtr StringValueCast(const ValuePtr& a)
	{ return std::dynamic_pointer_cast<const StringValue>(a); }
static inline const ValuePtr ValueCast(const StringValuePtr& sv)
	{ return std::shared_ptr<Value>(sv, (Value*) sv.get()); }

template<typename ... Type>
static inline std::shared_ptr<StringValue> createStringValue(Type&&... args) {
	return std::make_shared<StringValue>(std::forward<Type>(args)...);
}


/** @}*/
} // namespace opencog

#endif // _OPENCOG_STRING_VALUE_H
