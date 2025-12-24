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
#include <opencog/atoms/value/VoidValue.h>
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/util/oc_assert.h>

using namespace opencog;

// ==============================================================

SortedStream::SortedStream(const Handle& h)
	: RelationalValue(SORTED_STREAM, h)
{
}

SortedStream::SortedStream(const HandleSeq& hs)
	: RelationalValue(SORTED_STREAM, hs.at(0))
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
SortedStream::SortedStream(const ValueSeq& vsq)
	: RelationalValue(SORTED_STREAM, HandleCast(vsq.at(0)))
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

SortedStream::~SortedStream()
{
	if (not is_closed())
		close();
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
	if (0 == vp->size())
	{
		close();
		return;
	}

	_scratch->clear();
	_set.insert(vp);
}

void SortedStream::add(ValuePtr&& vp)
{
	if (0 == vp->size())
	{
		close();
		return;
	}

	_scratch->clear();
	_set.insert(std::move(vp));
}

// ==============================================================

// Use the provided schema to perform pair-wise compare.
bool SortedStream::less(const Value& lhs, const Value& rhs) const
{
	return compare(lhs, rhs);
}

// ==============================================================

/// Return just ONE item from the stream. If stream is empty, block.
/// If stream is closed, return empty LinkValue
void SortedStream::update() const
{
	// Get the latest from upstream.
	drain();

	// Try to remove one item from the set.
	ValuePtr val;
	if (const_cast<SortedStream*>(this)->_set.try_get(val))
	{
		ValueSeq vsq({val});
		_value.swap(vsq);
		return;
	}

	// Stream is empty. If its closed, then we hit end of stream.
	if (is_closed())
	{
		_value.clear();
		return;
	}

	// If we are here, then the stream is open but empty. Block and
	// wait for something to arrive or for the stream to close.
	try
	{
		const_cast<SortedStream*>(this)->_set.wait_get(val);
		ValueSeq vsq({val});
		_value.swap(vsq);
		return;
	}
	catch (typename concurrent_set<ValuePtr, ValueComp>::Canceled& e)
	{}

	// If we are here, the queue closed, with nothing in it.
	// So this is end-of-stream, again.
	_value.clear();
}

ValuePtr SortedStream::remove(void)
{
	// Grab whatever we can from upstream.
	drain();

	// If we are closed, then use update() to get one item at a time.
	// We don't do this when open, because update() will block in this
	// case.
	if (is_closed())
	{
		update();
		if (0 == _value.size())
			return createVoidValue();

		return _value[0];
	}
	return RelationalValue::remove();
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
