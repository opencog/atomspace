/*
 * opencog/atoms/value/RelationalValue.cc
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

#include <opencog/atoms/value/RelationalValue.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

// ==============================================================

RelationalValue::RelationalValue(Type t, const Handle& schema)
	: UnisetValue(t), _schema(schema)
{
	init_schema();
}

RelationalValue::~RelationalValue()
{
}

// ==============================================================

// Set up the comparison machinery by wrapping the given schema in
// an ExecutionOutputLink, fed by a pair of ValueShims that will pass
// the Values into the schema.
void RelationalValue::init_schema(void)
{
	// ValueShims for left and right comparison arguments
	_left_shim = createValueShimLink();
	_right_shim = createValueShimLink();

	// The ExecutionOutputLink provides general machinery that can run
	// the schema on the pair to be compared.
	_exout =
		createLink(HandleSeq({
			_schema,
			createLink(HandleSeq({
				HandleCast(_left_shim),
				HandleCast(_right_shim)}),
				LIST_LINK)}),
			EXECUTION_OUTPUT_LINK);

	// Scratch space in which temporaries are evaluated.
	_scratch = createAtomSpace(_schema->getAtomSpace());
}

// ==============================================================

// Use the provided schema to compare two values.
bool RelationalValue::compare(const Value& lhs, const Value& rhs) const
{
	// Set the values to be compared in the shims.
	_left_shim->set_value(ValuePtr(const_cast<Value*>(&lhs), [](Value*){}));
	_right_shim->set_value(ValuePtr(const_cast<Value*>(&rhs), [](Value*){}));

	_scratch->clear();
	return _exout->bevaluate(_scratch.get());
}

// ==============================================================
