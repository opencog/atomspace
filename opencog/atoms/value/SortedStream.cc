/*
 * opencog/atoms/value/SortedStream.cc
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

#include <opencog/atoms/value/SortedStream.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Transient.h>

using namespace opencog;

// ==============================================================

SortedStream::SortedStream(const Handle& h)
	: UnisetValue(SORTED_STREAM), _schema(h)
{
	init_cmp();
}

SortedStream::SortedStream(const HandleSeq& hs)
	: UnisetValue(SORTED_STREAM)
{
	if (2 != hs.size())
		throw SyntaxException(TRACE_INFO, "Expecting two handles!");

	_schema = hs[0];
	init_cmp();

	if (hs[1]->is_executable())
		init_src(hs[1]->execute());
	else
		init_src(hs[1]);
}

// Same as above, but the two arguments arive in a ValueSeq.
// Ideally, the factory *should* have given us a pair of Handles,
// but twiddling this is a hassle, so not doing that right now.
// XXX FIXME maybe fix the factory, some day.
SortedStream::SortedStream(const ValueSeq& vsq)
	: UnisetValue(SORTED_STREAM)
{
	if (2 != vsq.size() or
	    (not vsq[0]->is_atom()) or
	    (not vsq[1]->is_atom()))
		throw SyntaxException(TRACE_INFO, "Expecting two handles!");

	_schema = HandleCast(vsq[0]);
	init_cmp();

	Handle src = HandleCast(vsq[1]);
	if (src->is_executable())
		init_src(src->execute());
	else
		init_src(vsq[1]);
}

// Set up the compare op by wrapping the given relation in
// an ExecutionOutputLink, fed by a pair of ValueShims that
// will pass the Values into the relation.
void SortedStream::init_cmp(void)
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

// Set up all finite streams here. Finite streams get copied into
// the collection once and once only, and that's it.
void SortedStream::init_src(const ValuePtr& src)
{
	// Copy Link contents into the collection.
	if (src->is_type(LINK))
	{
		for (const Handle& h: HandleCast(src)->getOutgoingSet())
			UnisetValue::add(h);
		close();
		return;
	}

	// Everything else is just a collection of size one.
	// Possible future extensions:
	// If _source is an ObjectNode, then send *-read-* message ???
	// If source is a FloatStream or StringStream ... ???
	if (not src->is_type(LINK_VALUE))
	{
		UnisetValue::add(src);
		close();
		return;
	}

	// One-shot, non-streaming finite LinkValue
	if (not src->is_type(STREAM_VALUE))
	{
		ValueSeq vsq = LinkValueCast(src)->value();
		close();
		return;
	}

	// XXX TODO Finish Me: if we are here then its either a plain
	// stream or a ContainerValue. Create a thread, sit in that thread
	// and pull stuff. This need high-low watermarks to be added to
	// the cogutils, first.
}

SortedStream::~SortedStream()
{
	release_transient_atomspace(_scratch);
}

// ==============================================================

// Clear the transient before each use. That way, the base
// AtomSpace always provides accurate context for the schema.
// We need to do this only once per add, and not once per
// less(), There will be, in general log(N) calls to less for
// a SortedStream of size N. Or so one would hope. But the impl
// under the covers is std::set<> and it seems to be calling
// 2x that, because I guess it has no operator==() to work with.

/// Add one item to the stream. If the item is a VoidValue
/// or an empty LinkValue, the stream closes.
void SortedStream::add(const ValuePtr& vp)
{
	if ((vp->get_type() == VOID_VALUE) or
	    (vp->is_type(LINK_VALUE) and 0 == vp->size()))
	{
		close();
		return;
	}

	_scratch->clear_transient();
	UnisetValue::add(vp);
}

void SortedStream::add(ValuePtr&& vp)
{
	if ((vp->get_type() == VOID_VALUE) or
	    (vp->is_type(LINK_VALUE) and 0 == vp->size()))
	{
		close();
		return;
	}

	_scratch->clear_transient();
	UnisetValue::add(std::move(vp));
}

// ==============================================================

// Use the provided schema to perform pair-wise compare.
bool SortedStream::less(const Value& lhs, const Value& rhs) const
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

/// Return just ONE item from the stream. If stream is empty, block.
/// If stream is closed, return empty LinkValue
void SortedStream::update() const
{
	ValuePtr vp = const_cast<SortedStream*>(this)->remove();

	// Zero size means we've closed. End-of-stream.
	if (0 == vp->size())
	{
		_value.clear();
		return;
	}

	// Return just one item.
	ValueSeq vsq({vp});
	_value.swap(vsq);
	return;
}

// ==============================================================

std::string SortedStream::to_string(const std::string& indent) const
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

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(SORTED_STREAM, createSortedStream, const Handle&)
DEFINE_VALUE_FACTORY(SORTED_STREAM, createSortedStream, const HandleSeq&)
DEFINE_VALUE_FACTORY(SORTED_STREAM, createSortedStream, const ValueSeq&)
