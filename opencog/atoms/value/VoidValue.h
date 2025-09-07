/*
 * opencog/atoms/value/VoidValue.h
 *
 * Copyright (C) 2020 OpenCog Foundation
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

#ifndef _OPENCOG_VOIDVALUE_H
#define _OPENCOG_VOIDVALUE_H

#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * VoidValue represents no result of the function.
 */
class VoidValue : public Value
{
private:
	VoidValue() : Value(VOID_VALUE) {}

public:
	virtual ~VoidValue() {}

	static const ValuePtr INSTANCE;

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent) const {
		return indent + "(VoidValue)";
	}

	/** Returns true if the two atoms are equal.  */
	virtual bool operator==(const Value& other) const {
		return this == &other;
	}
};

template<typename ... Type>
static inline ValuePtr createVoidValue(void)
	{ return VoidValue::INSTANCE; }

/** @}*/
} // namespace opencog

#endif // _OPENCOG_VOIDVALUE_H
