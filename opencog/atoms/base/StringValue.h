/*
 * opencog/atoms/base/StringValue.h
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

#ifndef _OPENCOG_STRING_VALUE_H
#define _OPENCOG_STRING_VALUE_H

#include <vector>
#include <opencog/atoms/base/ProtoAtom.h>
#include <opencog/atoms/base/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * StringValues hold an ordered vector of doubles.
 */
class StringValue
	: public ProtoAtom
{
protected:
	std::vector<std::string> _value;

public:
	StringValue(std::string v) : ProtoAtom(STRING_VALUE) { _value.push_back(v); }
	StringValue(std::vector<std::string> v) : ProtoAtom(STRING_VALUE), _value(v) {}

	virtual ~StringValue() {}

	std::vector<std::string>& value() { return _value; }

	/** Returns a string representation of the value.
	 *
	 * @return A string representation of the value.
	 */
	virtual std::string toString(const std::string& indent);
	virtual std::string toShortString(const std::string& indent)
	{ return toString(indent); }

	/** Returns whether two atoms are equal.
	 *
	 * @return true if the atoms are equal, false otherwise.
	 */
	virtual bool operator==(const ProtoAtom&) const;
};

typedef std::shared_ptr<StringValue> StringValuePtr;
static inline StringValuePtr StringValueCast(const ProtoAtomPtr& a)
	{ return std::dynamic_pointer_cast<StringValue>(a); }

// XXX temporary hack ...
#define createStringValue std::make_shared<StringValue>


/** @}*/
} // namespace opencog

#endif // _OPENCOG_STRING_VALUE_H
