/*
 * opencog/atoms/proto/LinkValue.cc
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

#include <opencog/atoms/proto/LinkValue.h>
#include <opencog/atoms/proto/ValueFactory.h>

using namespace opencog;

bool LinkValue::operator==(const ProtoAtom& other) const
{
	if (LINK_VALUE != other.get_type()) return false;

	const LinkValue* lov = (const LinkValue*) &other;

	if (_value.size() != lov->_value.size()) return false;

	// Content-compare, NOT pointer-compare!
	size_t len = _value.size();
	for (size_t i=0; i<len; i++)
		if (*(_value[i]) != *(lov->_value[i])) return false;
	return true;
}

bool LinkValue::operator<(const ProtoAtom& other) const
{
	if (LINK_VALUE != other.get_type())
		return LINK_VALUE < other.get_type();

	const LinkValue* lov = (const LinkValue*) &other;

	return _value < lov->_value;

//auto it1 = _value.begin();
//auto it2 = other._value.begin();
//	while (it1 != _value.end() && it2 != other._value.end())
//	{
//		if (*it1 != *it2)
//			return *it1 < *it2;
//	}
}
// ==============================================================

std::string LinkValue::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type) + "\n";
	for (ProtoAtomPtr v :_value)
		rv += std::string(" ") + v->to_string(indent + "   ");
	rv += ")\n";
	return rv;
}

// Adds factory when library is loaded.
static __attribute__ ((constructor)) void init(void)
{
    valuefactory().addFactory(LINK_VALUE, (CreateProto) & (createLinkValue<std::vector<ProtoAtomPtr>>),
                                    std::vector<std::type_index> {std::type_index(typeid(std::vector<ProtoAtomPtr>))});
}

