/*
 * opencog/atoms/value/PtrValue.h
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

#ifndef _OPENCOG_PTRVALUE_H
#define _OPENCOG_PTRVALUE_H

#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * PtrValue holds an pointer to the native object. Typical usecase is passing
 * object to the GroundedSchemaNode for processing.
 */
class PtrValue
	: public Value
{
protected:
	std::shared_ptr<void> ptr;

public:
	/** Function to be called when value is destroyed to release the pointer */
	using Deleter = std::function<void(void*)>;

	PtrValue(void* ptr, Deleter deleter)
		: Value(PTR_VALUE), ptr(ptr, deleter) {}

	virtual ~PtrValue() {}

	/** Returns the pointer */
	void* value() const { return ptr.get(); }

	/** Returns a string representation of the value.  */
	virtual std::string to_string(const std::string& indent) const;

	/** Returns true if the two atoms are equal.  */
	virtual bool operator==(const Value&) const;
};

typedef std::shared_ptr<const PtrValue> PtrValuePtr;
static inline PtrValuePtr PtrValueCast(const ValuePtr& a)
{
	return std::dynamic_pointer_cast<const PtrValue>(a);
}

template<typename ... Type>
static inline std::shared_ptr<PtrValue> createPtrValue(Type&&... args)
{
	return std::make_shared<PtrValue>(std::forward<Type>(args)...);
}

/** @}*/
} // namespace opencog

#endif // _OPENCOG_PTRVALUE_H
