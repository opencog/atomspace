/*
 * opencog/atoms/value/GroupValue.h
 *
 * Copyright (C) 2025 BrainyBlaze LLC
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
#include <opencog/atoms/value/UnisetValue.h>
#include <opencog/atoms/flow/ValueShimLink.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * A streaming Value that groups items into buckets based on an
 * equivalence relation. The equivalence relation is defined by a
 * Lambda that takes two arguments and returns a crisp bool value,
 * indicating whether the two items are equivalent.
 *
 * Items that are equivalent are placed into the same bucket, where
 * each bucket is a UnisetValue. Items that match no existing bucket
 * cause a new bucket to be created.
 *
 * When the source stream closes, all buckets are closed and can be
 * retrieved one at a time via remove().
 */
class GroupValue
	: public UnisetValue
{
protected:
	// Equivalence schema and its evaluation
	Handle _schema;
	ValueShimLinkPtr _left_shim;
	ValueShimLinkPtr _right_shim;
	Handle _exout;
	AtomSpacePtr _scratch;

	void init_equiv(void);
	bool equivalent(const Value& lhs, const Value& rhs) const;

public:
	GroupValue(const Handle&);
	virtual ~GroupValue();

	virtual void add(const ValuePtr&) override;
	virtual void add(ValuePtr&&) override;
	virtual std::string to_string(const std::string& indent = "") const;
};

VALUE_PTR_DECL(GroupValue);
CREATE_VALUE_DECL(GroupValue);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_GROUP_VALUE_H
