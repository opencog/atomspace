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

	// The ExecutionOutputLink provides us with general machinery
	// that can run the schema on the pair to be compared. The
	// only gotcha here is that the shims cannot be placed in any
	// AtomSpace, and thus theExOutLink can't be, either. So far,
	// that's OK. There's also an annoying meta-issue: the schema
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

	// Scratch space in which temporaries are evaluated. It overlays
	// the AtomSpace in which the schema lives, and thus the schema has
	// access to this, to use as context.
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

	// Set silent=true to catch SilentException.
	return  _exout->bevaluate(_scratch.get(), true);
}

// ==============================================================

// Clear the transient before each use. That way, the base
// AtomSpace always provides accurate context for the schema.
// We need to do this only once per add, and not once per
// less(). There will be, in general, log(N) calls to less for
// a SortedStream of size N. Or so one would hope. But the impl
// under the covers is std::set<> and it seems to be calling
// 2x that, because I guess it has no operator==() to work with.

/// Add one item to the stream. If the item is a VoidValue
/// or an empty LinkValue, the stream closes.
void RelationalValue::add(const ValuePtr& vp)
{
	if (0 == vp->size())
	{
		close();
		return;
	}

	_scratch->clear();

	// The comparison relation can fail when the place to
	// get data from does not exist. Although  such a case
	// "should not happen", and "it's the user's fault", in practice
	// it seems that the reasons are benign, and we shouldn't tank
	// the pipeline because of this. The most reasonable thing I can
	// think of is just not add this element to the set; that is,
	// if there is no data, then pretend it does not exist.
	// So, catch the access exception, and skip adding it.
	// This will be a SilentException; other exceptions are real
	// and are not guarded against.
	//
	try {
		_set.insert(vp);
	} catch (const SilentException& ex) {}
}

void RelationalValue::add(ValuePtr&& vp)
{
	if (0 == vp->size())
	{
		close();
		return;
	}

	// See notes above.
	_scratch->clear();
	try {
		_set.insert(std::move(vp));
	} catch (const SilentException& ex) {}
}

// ==============================================================

std::string RelationalValue::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	rv += "\n";
	rv += _schema->to_short_string(indent + "   ");
	if (_source)
		rv += _source->to_short_string(indent + "   ");
	rv += ")\n";
	rv += indent + "; Currently:\n";
	rv += LinkValue::to_string(indent + "; ", LINK_VALUE);
	return rv;
}

// ==============================================================
