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
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/flow/ValueShimLink.h>
#include <opencog/atoms/core/FunctionLink.h>

using namespace opencog;

// ==============================================================

SortedValue::SortedValue(const Handle& h)
	: UnisetValue(SORTED_VALUE), _schema(h)
{
	// Create ValueShimLinks for left and right comparison arguments
	_left_shim = createValueShimLink();
	_right_shim = createValueShimLink();

	// Create ExecutionOutputLink: (ExecutionOutput _schema (List _left_shim _right_shim))
	Handle list_args = createLink(HandleSeq{_left_shim, _right_shim}, LIST_LINK);
	_exout = createLink(HandleSeq{_schema, list_args}, EXECUTION_OUTPUT_LINK);

	// Set remains open for adding values
}

// ==============================================================

bool SortedValue::less(const Value& lhs, const Value& rhs) const
{
	// Set the values on the shims
	ValueShimLinkPtr left_shim = ValueShimLinkCast(_left_shim);
	ValueShimLinkPtr right_shim = ValueShimLinkCast(_right_shim);

	left_shim->set_value(ValuePtr(const_cast<Value*>(&lhs), [](Value*){}));
	right_shim->set_value(ValuePtr(const_cast<Value*>(&rhs), [](Value*){}));

	// Execute the comparison
	FunctionLinkPtr exout_func = FunctionLinkCast(_exout);
	ValuePtr result = exout_func->execute();

	// Print the result for debugging
	printf("SortedValue::less() comparing:\n");
	printf("  lhs: %s\n", lhs.to_short_string().c_str());
	printf("  rhs: %s\n", rhs.to_short_string().c_str());
	printf("  result: %s\n", result->to_string().c_str());

	// For now, fall back to default comparison
	return UnisetValue::less(lhs, rhs);
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(SORTED_VALUE, createSortedValue, const Handle&)
