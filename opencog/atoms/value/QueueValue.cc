/*
 * opencog/atoms/value/QueueValue.cc
 *
 * Copyright (C) 2015, 2016 Linas Vepstas
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

#include <opencog/atoms/value/QueueValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

const Handle QueueValue::QUEUE_VALUE_KEY = createNode(CONCEPT_NODE, "QUEUE_VALUE_KEY");

bool QueueValue::operator==(const Value& other) const
{
	if (QUEUE_VALUE != other.get_type()) return false;

	const QueueValue* cv = (const QueueValue*) &other;
	return _name == cv->_name;
}

std::string QueueValue::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	rv += std::string(" \"") + _name + "\")";
	return rv;
}

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(QUEUE_VALUE,
                     createQueueValue, std::string)
