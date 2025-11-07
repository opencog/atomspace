/*
 * opencog/atoms/value/SortedValue.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, Inc.
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

#ifndef _OPENCOG_SORTED_VALUE_H
#define _OPENCOG_SORTED_VALUE_H

#include <opencog/atoms/value/UnisetValue.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * SortedValues provide a thread-safe sorted set of Values with
 * custom comparison logic. They extend UnisetValue by overriding
 * the less() method to provide custom sorting behavior.
 *
 * Elements are deduplicated and maintained in sorted order according
 * to the custom comparison function.
 */
class SortedValue
	: public UnisetValue
{
protected:
	SortedValue(Type t) : UnisetValue(t) {}

	/**
	 * Override the comparison method to provide custom sorting.
	 * This method determines the ordering of Values in the set.
	 *
	 * @param lhs The left-hand side Value to compare
	 * @param rhs The right-hand side Value to compare
	 * @return true if lhs should come before rhs in sorted order
	 */
	virtual bool less(const Value& lhs, const Value& rhs) const override;

public:
	SortedValue(void) : UnisetValue(SORTED_VALUE) {}
	SortedValue(const ValueSeq&);
	virtual ~SortedValue() {}
};

VALUE_PTR_DECL(SortedValue);
CREATE_VALUE_DECL(SortedValue);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_SORTED_VALUE_H
