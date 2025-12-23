/*
 * opencog/atoms/value/GroupValue.cc
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, LLC
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

#include <opencog/atoms/value/GroupValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

// ==============================================================

GroupValue::GroupValue(const Handle& h)
	: RelationalValue(GROUP_VALUE, h)
{
}

GroupValue::~GroupValue()
{
}

// ==============================================================

// Use the provided schema to test if two values are equivalent.
bool GroupValue::equivalent(const Value& lhs, const Value& rhs) const
{
	return compare(lhs, rhs);
}

// ==============================================================

void GroupValue::add(const ValuePtr& vp)
{
	add(ValuePtr(vp));
}

/// Add one item to the stream. The item is placed into a bucket
/// with other equivalent items. If no equivalent bucket exists,
/// a new bucket is created. If the item is a VoidValue or an
/// empty LinkValue, the stream closes.
void GroupValue::add(ValuePtr&& vp)
{
	// VoidValue or empty LinkValue signals end-of-stream.
	if ((vp->get_type() == VOID_VALUE) or
	    (vp->is_type(LINK_VALUE) and 0 == vp->size()))
	{
		// Close all open buckets before closing the stream.
		for (const ValuePtr& bucket : _set.snapshot())
			UnisetValueCast(bucket)->close();
		close();
		return;
	}

	_scratch->clear();

	// Search existing buckets for an equivalent item.
	for (const ValuePtr& bucket : _set.snapshot())
	{
		UnisetValuePtr uvp = UnisetValueCast(bucket);
		ValuePtr rep = uvp->peek();
		if (nullptr == rep) continue;

		if (equivalent(*vp, *rep))
		{
			uvp->add(std::move(vp));
			return;
		}
	}

	// No equivalent bucket found; create a new one.
	UnisetValuePtr newbucket = createUnisetValue();
	newbucket->add(std::move(vp));
	_set.insert(newbucket);
}

// ==============================================================

void GroupValue::close(void)
{
	// Close all open buckets before closing the stream.
	for (const ValuePtr& bucket : _set.snapshot())
		UnisetValueCast(bucket)->close();
	UnisetValue::close();
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(GROUP_VALUE, createGroupValue, const Handle&)
