/*
 * opencog/atoms/value/ProducerConsumerValue.cc
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

#include <opencog/atoms/value/ProducerConsumerValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

const Handle ProducerConsumerValue::CONTROL_KEY = createNode(CONCEPT_NODE, "PRODUCER_CONSUMER_KEY");

bool ProducerConsumerValue::operator==(const Value& other) const
{
	if (PRODUCER_CONSUMER_VALUE != other.get_type()) return false;

	const ProducerConsumerValue* cv = (const ProducerConsumerValue*) &other;
	return _name == cv->_name;
}

std::string ProducerConsumerValue::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	rv += std::string(" \"") + _name + "\")";
	return rv;
}

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(PRODUCER_CONSUMER_VALUE,
                     createProducerConsumerValue, std::string)


// ==============================================================

void ProducerConsumerControl::produce(const Handle& h)
{
	_queue.push(h);
}

void ProducerConsumerControl::subscribe(Consumer consume)
{
	_consume = consume;
}

void ProducerConsumerControl::finished()
{
	if (!_consume) return;

	while (!_queue.empty())
	{
		_consume(_queue.front());
		_queue.pop();
	}
}
