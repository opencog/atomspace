/*
 * opencog/atoms/value/StringValue.cc
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

#include <iomanip>

#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

bool StringValue::operator==(const Value& other) const
{
	if (STRING_VALUE != other.get_type()) return false;

	const StringValue* sov = (const StringValue*) &other;

	if (_value.size() != sov->_value.size()) return false;
	size_t len = _value.size();
	for (size_t i=0; i<len; i++)
		if (_value[i] != sov->_value[i]) return false;
	return true;
}

// ==============================================================

/// Print the StringValue. Escape any quotes in the strings when
/// printing. This is needed for readability, since this is an
/// array of strings, and without the escapes, we can't tell where
/// strings start and end.
std::string StringValue::to_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent << "(" << nameserver().getTypeName(_type);
	for (const std::string& v :_value)
		ss << " " << std::quoted(v);
	ss << ")";
	return ss.str();
}

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(STRING_VALUE,
                     createStringValue, std::vector<std::string>)
DEFINE_VALUE_FACTORY(STRING_VALUE,
                     createStringValue, std::string)
