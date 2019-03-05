/*
 * opencog/atoms/value/PtrValue.cc
 *
 * Copyright (C) 2019 Vitaly Bogdanov
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

#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atoms/value/PtrValue.h>

using namespace opencog;

bool PtrValue::operator==(const Value& other) const
{
	if (PTR_VALUE != other.get_type()) return false;

	const PtrValue* value = dynamic_cast<const PtrValue*>(&other);

	return this->value() == value->value();
}

// ==============================================================

std::string PtrValue::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	{
		char buf[20];
		snprintf(buf, 20, "%p", value());
		rv += std::string(" ") + buf;
	}
	rv += ")\n";
	return rv;
}

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(PTR_VALUE, createPtrValue, void*, PtrValue::Deleter)
