/*
 * opencog/atoms/value/SortedValue.cc
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

#include <opencog/atoms/value/SortedValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

// ==============================================================

SortedValue::SortedValue(const Handle& h)
	: RelationalValue(SORTED_VALUE, h)
{
}

SortedValue::SortedValue(const HandleSeq& hs)
	: RelationalValue(SORTED_VALUE, hs.at(0))
{
	if (2 != hs.size())
		throw SyntaxException(TRACE_INFO, "Expecting two handles!");

	if (hs[1]->is_executable())
		init_src(hs[1]->execute());
	else
		init_src(hs[1]);
}

// Same as above, but the two arguments arive in a ValueSeq.
// Ideally, the factory *should* have given us a pair of Handles,
// but twiddling this is a hassle, so not doing that right now.
// XXX FIXME maybe fix the factory, some day.
SortedValue::SortedValue(const ValueSeq& vsq)
	: RelationalValue(SORTED_VALUE, HandleCast(vsq.at(0)))
{
	if (2 != vsq.size() or
	    (not vsq[0]->is_atom()) or
	    (not vsq[1]->is_atom()))
		throw SyntaxException(TRACE_INFO, "Expecting two handles!");

	Handle src = HandleCast(vsq[1]);
	if (src->is_executable())
		init_src(src->execute());
	else
		init_src(vsq[1]);
}

SortedValue::~SortedValue()
{
}

// ==============================================================

// Use the provided schema to perform pair-wise compare.
bool SortedValue::less(const Value& lhs, const Value& rhs) const
{
	return compare(lhs, rhs);
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(SORTED_VALUE, createSortedValue, const Handle&)
DEFINE_VALUE_FACTORY(SORTED_VALUE, createSortedValue, const HandleSeq&)
DEFINE_VALUE_FACTORY(SORTED_VALUE, createSortedValue, const ValueSeq&)
