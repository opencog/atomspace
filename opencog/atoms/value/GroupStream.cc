/*
 * opencog/atoms/value/GroupStream.cc
 *
 * Copyright (C) 2025 BrainyBlaze LLC
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

#include <opencog/atoms/value/GroupStream.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

// ==============================================================

GroupStream::GroupStream(const Handle& h)
	: UnisetValue(GROUP_STREAM), _schema(h)
{
	init_equiv();
}

GroupStream::~GroupStream()
{
}

// ==============================================================

void GroupStream::init_equiv(void)
{
	// TODO: Set up the equivalence comparator
}

// ==============================================================

bool GroupStream::equivalent(const Value& lhs, const Value& rhs) const
{
	// TODO: Evaluate the schema to determine equivalence
	return false;
}

// ==============================================================

void GroupStream::add(const ValuePtr& vp)
{
	// TODO: Assign item to appropriate bucket
}

void GroupStream::add(ValuePtr&& vp)
{
	// TODO: Assign item to appropriate bucket
}

// ==============================================================

void GroupStream::update() const
{
	// TODO: Return next bucket
}

// ==============================================================

std::string GroupStream::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	rv += "\n";
	rv += _schema->to_short_string(indent + "   ");
	rv += ")\n";
	return rv;
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(GROUP_STREAM, createGroupStream, const Handle&)
