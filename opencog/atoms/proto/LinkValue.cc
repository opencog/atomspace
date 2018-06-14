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

// ==============================================================

std::string LinkValue::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type) + "\n";
	for (ProtoAtomPtr v :_value)
		rv += std::string(" ") + v->to_string(indent + "   ");
	rv += ")\n";
	return rv;
}
