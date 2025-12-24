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
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/util/oc_assert.h>

using namespace opencog;

// ==============================================================

SortedStream::SortedStream(const Handle& h)
	: RelationalValue(SORTED_STREAM, h), _source(nullptr)
{
}

SortedStream::SortedStream(const HandleSeq& hs)
	: RelationalValue(SORTED_STREAM, hs.at(0)), _source(nullptr)
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
	: RelationalValue(SORTED_STREAM, HandleCast(vsq.at(0))), _source(nullptr)
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

// Set up all finite streams here. Finite streams get copied into
// the collection once and once only, and that's it. They're sorted,
// and then deliverd one at a time from the buffer.
void SortedStream::init_src(const ValuePtr& src)
{
	// Copy Link contents into the collection.
	if (src->is_type(LINK))
	{
		for (const Handle& h: HandleCast(src)->getOutgoingSet())
			_set.insert(h);
		_set.close();
		return;
	}

	// Everything else is just a collection of size one.
	// Possible future extensions:
	// If _source is an ObjectNode, then send *-read-* message ???
	// If source is a FloatStream or StringStream ... ???
	if (not src->is_type(LINK_VALUE))
	{
		_set.insert(src);
		_set.close();
		return;
	}

	// One-shot, non-streaming finite LinkValue
	if (not src->is_type(CONTAINER_VALUE) and
	    not src->is_type(STREAMING_SIG) and
	    not src->is_type(HOARDING_SIG))
	{
		ValueSeq vsq = LinkValueCast(src)->value();
		for (const ValuePtr& vp: vsq)
			_set.insert(vp);
		_set.close();
		return;
	}

	// If it's a container, and its closed, then its a finite,
	// one-shot deal, just like the above.
	if (src->is_type(CONTAINER_VALUE) and
	    ContainerValueCast(src)->is_closed())
	{
		ValueSeq vsq = LinkValueCast(src)->value();
		for (const ValuePtr& vp: vsq)
			_set.insert(vp);
		_set.close();
		return;
	}

	// If we are here, then the data source is either a StreamingSig
	// or a HaoardingSig. These are potentially infinite sources,
	// they typically block during reading; so we cannot handle them
	// here. Instead, these will be polled later, during update().
	_source = LinkValueCast(src);
}

void SortedStream::drain(void) const
{
	// Plain streams are easy. Just sample and go.
	if (not _source->is_type(CONTAINER_VALUE))
	{
		// Use source size() as a surrogate to tell us if the source
		// will block. Pull as much as we can, without blocking.
		while (0 < _source->size())
		{
			ValueSeq vsq = _source->value();

			// Zero-sized sequences (e.g. VoidValue) indicate end-of-stream.
			if (0 == vsq.size())
			{
				_set.close();
				return;
			}

			// !!??? We flatten here. Is this correct?
			for (const ValuePtr& vp: vsq)
				_set.insert(vp);
		}
		return;
	}

	// If we are here, we've got a container ... It needs to be drained
	// one at a time.
	ContainerValuePtr cvp = ContainerValueCast(_source);
	while (0 < cvp->size() or cvp->is_closed())
	{
#if IS_THIS_NEEDED
		ValuePtr vp;
		try
		{
			vp = cvp->remove();
		}
		catch (typename concurrent_set<ValuePtr, ValueComp>::Canceled& e)
		{
			_set.close(); // We are done; close shop.
			return;
		}
#endif

		ValuePtr vp = cvp->remove();

		// Zero-sized sequences (e.g. VoidValue) indicate end-of-stream.
		if (0 == vp->size())
		{
			_set.close();
			return;
		}

		_set.insert(vp);
	}
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
	if (_source) drain();

	// Try to grabl one item from the set.
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
