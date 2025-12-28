/*
 * opencog/atoms/value/GroupValue.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamcis, LLC
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
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef _OPENCOG_GROUP_VALUE_H
#define _OPENCOG_GROUP_VALUE_H

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/value/RelationalValue.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * A ContainerValue that groups items into buckets based on an
 * equivalence relation. The equivalence relation cane be any
 * Atomese function that evaluates to crisp true/false when given
 * a pair of input values.
 *
 * The canonical form of the equivalence relation is as a LambdaLink:
 *
 *   (Lambda
 *      (VariableList (Variable $A) (Variable $B))
 *      (Equal
 *          (Some expr that fishes a Value out of $A)
 *          (Some expr that fishes a Value out of $B)))
 *
 * Equivalent items are collected up into buckets. Usage patterns
 * are as for generic ContainerValues: items may be added at any time;
 * its thread safe. The Container is created open, and items can be
 * added up until it is closed.
 */
class GroupValue
	: public RelationalValue
{
public:
	GroupValue(const Handle&);
	virtual ~GroupValue();

	virtual void add(const ValuePtr&) override;
	virtual void add(ValuePtr&&) override;
	virtual void close(void) override;
};

VALUE_PTR_DECL(GroupValue);
CREATE_VALUE_DECL(GroupValue);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_GROUP_VALUE_H
