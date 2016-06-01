/*
 * opencog/atoms/base/AssoValue.h
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

#ifndef _OPENCOG_ASSO_VALUE_H
#define _OPENCOG_ASSO_VALUE_H

#include <vector>
#include <opencog/atoms/base/ProtoAtom.h>
#include <opencog/atoms/base/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * AssoValues hold an bag of key-value pairs.
 */
class AssoValue
	: public ProtoAtom
{
protected:
	std::vector<std::string> _value;

public:
	AssoValue(std::string v) : ProtoAtom(ASSO_VALUE) { _value.push_back(v); }
	AssoValue(std::vector<std::string> v) : ProtoAtom(ASSO_VALUE), _value(v) {}

	virtual ~AssoValue() {}

	std::vector<std::string>& value() { return _value; }
	void setValue(const std::vector<std::string>& v) { _value = v; }
	void setValue(const std::string& v) {
		_value = std::vector<std::string>({v}); }


	/** Returns a string representation of the value.  */
	virtual std::string toAsso(const std::string& indent);
	virtual std::string toShortAsso(const std::string& indent)
	{ return toAsso(indent); }

	/** Returns true if the two atoms are equal.  */
	virtual bool operator==(const ProtoAtom&) const;
};

typedef std::shared_ptr<AssoValue> AssoValuePtr;
static inline AssoValuePtr AssoValueCast(const ProtoAtomPtr& a)
	{ return std::dynamic_pointer_cast<AssoValue>(a); }

// XXX temporary hack ...
#define createAssoValue std::make_shared<AssoValue>


/** @}*/
} // namespace opencog

#endif // _OPENCOG_ASSO_VALUE_H
