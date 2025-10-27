/*
 * opencog/atoms/value/FlatStream.cc
 *
 * Copyright (C) 2020, 2022 Linas Vepstas
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

#include <stdlib.h>
#include <opencog/atoms/value/FlatStream.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

// ==============================================================

FlatStream::FlatStream(const Handle& h) :
	LinkValue(FLAT_STREAM), _source(h), _as(h->getAtomSpace()),
	_current_stream(nullptr), _current_index(0)
{
	init();
}

FlatStream::FlatStream(const HandleSeq&& oset) :
	LinkValue(FLAT_STREAM)
{
	// XXX FIXME I guess we could flatten more than one
	// stream at a time. So it would be like sucking off
	// a row of stuff from a matrix of columns. I suppose
	// that's a plausible thing to do, but right now it is
	// not needed, so doing this is a future extension for
	// that day when it might be needed.
	if (1 != oset.size())
		throw SyntaxException(TRACE_INFO,
			"Expecting just one atom!");

	_source = oset[0];
	_as = oset[0]->getAtomSpace();

	init();
}

// A pseudo copy constructor. Just copies in some existing Link Value.
FlatStream::FlatStream(const ValuePtr& vp) :
	LinkValue(FLAT_STREAM),
	_source(nullptr),
	_as(nullptr),
	_current_index(0)
{
	if (not vp->is_type(LINK_VALUE))
		throw SyntaxException(TRACE_INFO,
			"Expecting a LinkValue, got %s",
			vp->to_string().c_str());

	_current_stream = LinkValueCast(vp);
}

void FlatStream::init(void)
{
	// Nothing, actually ...
}

// ==============================================================

void FlatStream::update() const
{
	// Are we done yet?
	if (_current_stream and 0 < _current_index and 0 == _value.size()) return;

	// Flatten
	if (_current_stream and _current_index < _current_stream->_value.size())
	{
		const ValuePtr& vp = _current_stream->_value[_current_index];
		if (vp->is_type(LINK_VALUE))
		{
			ValueSeq vsq(LinkValueCast(vp)->value());
			_value.swap(vsq);
			_current_index++;
			return;
		}

		// This case is hit when flattening is being done on an ordinary
		// Link. The _current_stream holds the outgoing set of the Link,
		// so all we have to do is present each Atom in that oset, one by
		// one.
		if (vp->is_type(ATOM))
		{
			ValueSeq vsq({vp});
			_value.swap(vsq);
			_current_index++;
			return;
		}

		// XXX A weird stupid computer trick here would be to iterate
		// over an entire AtomSpace. At the moment, this is awkward,
		// because it would require making a copy of all of the Handles.
		// That, plus nothing needs this ability. That, plus maybe
		// there should be some distinct way of stuffing an entire
		// AtomSpace into a distinct LinkValue. That, plus maybe there
		// should be a way of following AtomSpace changes. That, plus
		// the fact that ProxyNodes are intended for this kind of stuff.
		// So we don't handle the case of an AtomSpace, here.

		throw RuntimeException(TRACE_INFO,
			"Expecting a LinkValue or Link, got %s",
			vp->to_string().c_str());

	}

	// (Re-)Start at the begining.
	_current_index = 0;

	// Get the next list that needs flattening.
	// Normally, the source is executable, and executing it returns a list
	// of items that we will iterate over.
	// But if its a Link Atom , we handle it as a special case, a "trivial" case:
	// just iterate over the outgoing set of the Link itself.
	// As currently designed, this will loop forever, but maybe it should halt?
	if (not _source->is_executable())
	{
		ValueSeq hov;
		HandleSeq ho(_source->getOutgoingSet());
		for (const Handle& h : ho)
			hov.push_back(h);
		_current_stream = createLinkValue(std::move(hov));
		update(); // Loop around.
		return;
	}

	ValuePtr result = _source->execute(_as);

	// Check for end of stream
	if (nullptr == result or result->get_type() == VOID_VALUE)
	{
		ValueSeq empty;
		_value.swap(empty);
		return;
	}

	if (result->get_type() != LINK_VALUE)
	{
		ValueSeq unary({result});
		_value.swap(unary);
		return;
	}

	// Cast to LinkValue
	_current_stream = LinkValueCast(result);

	// We've queued it up; take it from the top.
	update();
}

// ==============================================================

std::string FlatStream::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	rv += "\n" + _source->to_short_string(indent + "   ") + ")\n";
	rv += indent + "; Currently:\n";
	rv += LinkValue::to_string(indent + "; ", LINK_VALUE);
	return rv;
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(FLAT_STREAM, createFlatStream, const Handle&)
DEFINE_VALUE_FACTORY(FLAT_STREAM, createFlatStream, const HandleSeq&&)
