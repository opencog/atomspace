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
	LinkValue(FLAT_STREAM), _source(h), _as(h->getAtomSpace())
{
	init();
}

void FlatStream::init(void)
{
	// Verify that we've got valid stuff.
	if (not _source->is_executable() and
	    not _source->is_evaluatable())
	{
		throw SyntaxException(TRACE_INFO,
			"Expecting an executable or evaluatable atom, got %s",
			_source->to_string().c_str());
	}
}

// ==============================================================

void FlatStream::update() const
{
	// Empty for now - to be implemented later
}

// ==============================================================

std::string FlatStream::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	rv += "\n" + _source->to_short_string(indent + "   ");
	rv += "\n)";
	return rv;
}

// ==============================================================

bool FlatStream::operator==(const Value& other) const
{
	if (FLAT_STREAM != other.get_type()) return false;

	const FlatStream* eso = (const FlatStream*) &other;
	return eso->_source == _source;
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(FLAT_STREAM, createFlatStream, const Handle&)
