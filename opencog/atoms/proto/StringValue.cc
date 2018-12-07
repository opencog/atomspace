/*
 * opencog/atoms/proto/StringValue.cc
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

#include <opencog/atoms/proto/StringValue.h>
#include <opencog/atoms/proto/ValueFactory.h>

using namespace opencog;

bool StringValue::operator==(const ProtoAtom& other) const
{
	if (STRING_VALUE != other.get_type()) return false;

	const StringValue* sov = (const StringValue*) &other;

	if (_value.size() != sov->_value.size()) return false;
	size_t len = _value.size();
	for (size_t i=0; i<len; i++)
		if (_value[i] != sov->_value[i]) return false;
	return true;
}

bool StringValue::operator<(const ProtoAtom& other) const
{
	if (STRING_VALUE != other.get_type())
		return STRING_VALUE < other.get_type();

	const StringValue* sov = (const StringValue*) &other;

	return _value < sov->_value;
}

// ==============================================================

std::string StringValue::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	for (std::string v :_value)
		rv += std::string(" \"") + v + "\"";
	rv += ")\n";
	return rv;
}

// Adds factory when library is loaded.
static __attribute__ ((constructor)) void init(void)
{
    valuefactory().addFactory(STRING_VALUE, (CreateProto) & (createStringValue<std::vector<std::string>>),
                                    std::vector<std::type_index> {std::type_index(typeid(std::vector<std::string>))});

    valuefactory().addFactory(STRING_VALUE, (CreateProto) & (createStringValue<std::string>),
                                    std::vector<std::type_index> {std::type_index(typeid(std::string))});
}
