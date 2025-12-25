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

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(SORTED_STREAM, createSortedStream, const Handle&)
DEFINE_VALUE_FACTORY(SORTED_STREAM, createSortedStream, const HandleSeq&)
DEFINE_VALUE_FACTORY(SORTED_STREAM, createSortedStream, const ValueSeq&)
