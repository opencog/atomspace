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
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Transient.h>

using namespace opencog;

// ==============================================================

SortedValue::SortedValue(const Handle& h)
	: UnisetValue(SORTED_VALUE), _schema(h)
{
	// ValueShims for left and right comparison arguments
	_left_shim = createValueShimLink();
	_right_shim = createValueShimLink();

	_exout =
		createLink(HandleSeq({
			_schema,
			createLink(HandleSeq({
				HandleCast(_left_shim),
				HandleCast(_right_shim)}),
				LIST_LINK)}),
			EXECUTION_OUTPUT_LINK);
	_scratch = grab_transient_atomspace(_schema->getAtomSpace());
}

SortedValue::~SortedValue()
{
	release_transient_atomspace(_scratch);
}

// ==============================================================

bool SortedValue::less(const Value& lhs, const Value& rhs) const
{
	// Ugly casts. But so it goes.
	_left_shim->set_value(ValuePtr(const_cast<Value*>(&lhs), [](Value*){}));
	_right_shim->set_value(ValuePtr(const_cast<Value*>(&rhs), [](Value*){}));

	ValuePtr result = _exout->execute(_scratch);

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
