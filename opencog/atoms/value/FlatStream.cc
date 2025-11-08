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
	LinkValue(FLAT_STREAM),
	_current_stream(nullptr), _current_index(0)
{
	if (not h->is_executable())
		throw SyntaxException(TRACE_INFO,
			"Expecting an executable source, got %s",
			h->to_string().c_str());

	init(h->execute());
}

FlatStream::FlatStream(const ValuePtr& vp) :
	LinkValue(FLAT_STREAM),
	_current_stream(nullptr), _current_index(0)
{
	init(vp);
}

void FlatStream::init(const ValuePtr& vp)
{
	if (not vp->is_type(LINK_VALUE))
		throw SyntaxException(TRACE_INFO,
			"Expecting a LinkValue, got %s",
			vp->to_string().c_str());

	_source = LinkValueCast(vp);
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

		// End-of-stream marker.
		if (vp->is_type(VOID_VALUE))
		{
			_value.clear(); // Set sequence size to zero...
			_current_index++;
			return;
		}

		ValueSeq vsq({vp});
		_value.swap(vsq);
		_current_index++;
		return;
	}

	// (Re-)Start at the begining.
	_current_index = 0;

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
	else if (_current_stream)
		rv += _current_stream->to_short_string(indent + "   ");
	rv += ")\n";
	rv += indent + "; Currently:\n";
	rv += LinkValue::to_string(indent + "; ", LINK_VALUE);
	return rv;
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(FLAT_STREAM, createFlatStream, const Handle&)
DEFINE_VALUE_FACTORY(FLAT_STREAM, createFlatStream, const ValuePtr&)
