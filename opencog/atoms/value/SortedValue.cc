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

	// The ExecutionOutputLink provides us with general machinery
	// that can run the schema on the pair to be compared. The
	// only gotcha here is that the shims cannot be placed in any
	// AtomSpace, and thus theExOutLink can't be, either. So far,
	// That's OK. There's also an annoying meta-issue: the schema
	// can't be applied without beta-reduction, which is ...
	// annoying. But that's the best we can do with the current
	// architecture. For now.
	_exout =
		createLink(HandleSeq({
			_schema,
			createLink(HandleSeq({
				HandleCast(_left_shim),
				HandleCast(_right_shim)}),
				LIST_LINK)}),
			EXECUTION_OUTPUT_LINK);

	// Scratch space in which temproaries are evaluated. This
	// overlays the AtomSpace in which the schema sits, and thus,
	// the schema can use this for context.
	_scratch = grab_transient_atomspace(_schema->getAtomSpace());
}

SortedValue::~SortedValue()
{
	release_transient_atomspace(_scratch);
}

// ==============================================================

// Clear the transient before each use. That way, the base
// AtomSpace always provides accurate context for the schema.
// We need to do this only once per add, and not once per
// less(), There will be, in general log(N) calls to less for
// a SortedValue of size N. Or so one would hope. But the impl
// under the covers is std::set<> and it seems to be calling
// 2x that, because I guess it has no operator==() to work with.
void SortedValue::add(const ValuePtr& vp)
{
	_scratch->clear_transient();
	UnisetValue::add(vp);
}

void SortedValue::add(ValuePtr&& vp)
{
	_scratch->clear_transient();
	UnisetValue::add(std::move(vp));
}

// ==============================================================

// Use the provided schema to perform pair-wise compare.
bool SortedValue::less(const Value& lhs, const Value& rhs) const
{
	// Ugly casts. But so it goes.
	_left_shim->set_value(ValuePtr(const_cast<Value*>(&lhs), [](Value*){}));
	_right_shim->set_value(ValuePtr(const_cast<Value*>(&rhs), [](Value*){}));

	ValuePtr vp = _exout->execute(_scratch);

	if (not vp->is_type(BOOL_VALUE))
		throw RuntimeException(TRACE_INFO,
			"Expecting BoolValue compare; got %s\n",
			vp->to_string().c_str());

	BoolValuePtr bv = BoolValueCast(vp);
	return bv->value()[0];
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(SORTED_VALUE, createSortedValue, const Handle&)
