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
	friend class TransposeColumn;

protected:
	mutable std::vector<std::string> _value;

	virtual void update() const {}
	std::string to_string(const std::string&, Type) const;

	StringValue(Type t, const std::vector<std::string>& v)
		: Value(t), _value(v) {}

public:
	StringValue(const std::string& v)
		: Value(STRING_VALUE) { _value.push_back(v); }
	StringValue(const std::vector<std::string>& v)
		: Value(STRING_VALUE), _value(v) {}

	virtual ~StringValue() {}

	const std::vector<std::string>& value() const { update(); return _value; }
	size_t size() const { return _value.size(); }

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent = "") const
	{ return to_string(indent, _type); }

	/** Returns true if the two atoms are equal.  */
	virtual bool operator==(const Value&) const;

	/** Optimized less-than comparison for StringValue.
	 * Compares by type first, then vector length, then individual strings.
	 * Much faster than the base class to_string() comparison. */
	virtual bool operator<(const Value& other) const;
};

VALUE_PTR_DECL(StringValue);
CREATE_VALUE_DECL(StringValue);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_STRING_VALUE_H
