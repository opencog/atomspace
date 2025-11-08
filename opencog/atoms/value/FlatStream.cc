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
	LinkValue(FLAT_STREAM)
{
	if (not h->is_executable())
		throw SyntaxException(TRACE_INFO,
			"Expecting an executable source, got %s",
			h->to_string().c_str());

	init(h->execute());
}

FlatStream::FlatStream(const ValuePtr& vp) :
	LinkValue(FLAT_STREAM)
{
	init(vp);
}

// Set up all finite streams here. Finite streams get copied into
// the collection once and once only, and that's it.
void FlatStream::init(const ValuePtr& vp)
{
	_index = 0;
	_source = vp;
	_collection = nullptr;

	// Copy Link contents into the collection.
	if (_source->is_type(LINK))
	{
		ValueSeq vsq;
		for (const Handle& h: HandleCast(_source)->getOutgoingSet())
			vsq.push_back(h);
		_collection = createLinkValue(vsq);
		return;
	}

	// Everything else is just a collection of size one.
	// Possible future extensions:
	// If _source is an ObjectNode, then send *-read-* message ???
	// If source is a FloatStream or StringStream ... ???
	if (not _source->is_type(LINK_VALUE))
	{
		_collection = createLinkValue(ValueSeq{_source});
		return;
	}

	// One-shot, non-streaming finite LinkValue
	if (not _source->is_type(STREAM_VALUE))
	_collection = LinkValueCast(_source);
}

// ==============================================================

void FlatStream::update() const
{
	// Are we done yet?
	if (_collection and 0 < _index and 0 == _value.size()) return;

	// Flatten
	if (_collection and _index < _collection->_value.size())
	{
		const ValuePtr& vp = _collection->_value[_index];

		// End-of-stream marker.
		if (vp->is_type(VOID_VALUE))
		{
			_value.clear(); // Set sequence size to zero...
			_index++;
			return;
		}

		ValueSeq vsq({vp});
		_value.swap(vsq);
		_index++;
		return;
	}

	// (Re-)Start at the begining.
	_index = 0;

	// End-file-file condition.
	if (nullptr == _source or VOID_VALUE == _source->get_type())
	{
		_value.clear(); // Set sequence size to zero...
		return;
	}
}

// ==============================================================

std::string FlatStream::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	rv += "\n";
	if (_source)
		rv += _source->to_short_string(indent + "   ");
	else if (_collection)
		rv += _collection->to_short_string(indent + "   ");
	rv += ")\n";
	rv += indent + "; Currently:\n";
	rv += LinkValue::to_string(indent + "; ", LINK_VALUE);
	return rv;
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(FLAT_STREAM, createFlatStream, const Handle&)
DEFINE_VALUE_FACTORY(FLAT_STREAM, createFlatStream, const ValuePtr&)
