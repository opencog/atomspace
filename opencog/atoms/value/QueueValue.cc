/*
 * opencog/atoms/value/QueueValue.cc
 *
 * Copyright (C) 2020 Linas Vepstas
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

// ==============================================================

QueueValue::QueueValue(const ValueSeq& vseq)
	: LinkStreamValue(QUEUE_VALUE)
{
	for (const ValuePtr& v: vseq)
		push(v); // concurrent_queue<ValutePtr>::push(v);

	// Since this constructor placed stuff on the queue,
	// we also close it, to indicate we are "done" placing
	// things on the queue. If some user needs to add more,
	// then they need to re-open.
	close();
}

// ==============================================================

// This will clear the return value, and then block until the
// writer closes the queue. Only then does this return. Upon
// return, all of the values that the writer ever wrote are
// in the list of values.
//
// Basically, the reader should open the queue, the writer should
// produce a bunch of values, and, when done, close the queue. The
// reader can then hoover them all up by calling LinkValue::value()
//
// Alternately, more clever users can work with the concurrent queue
// API directly; they do not need to go through this API.
void QueueValue::update() const
{
	// Do nothing; we don't want to clobber the _value
	if (is_closed() and 0 == concurrent_queue<ValuePtr>::size()) return;

	// Reset, to start with.
	_value.clear();

	// Loop forever, as long as the queue is open.
	try
	{
		while (true)
		{
			ValuePtr val;
			const_cast<QueueValue*>(this) -> pop(val);
			_value.emplace_back(val);
		}
	}
	catch (typename concurrent_queue<ValuePtr>::Canceled& e)
	{}

	// If we are here, the queue closed up. Reopen it
	// just long enough to drain any remaining values.
	const_cast<QueueValue*>(this) -> cancel_reset();
	while (not is_empty())
	{
		ValuePtr val;
		const_cast<QueueValue*>(this) -> pop(val);
		_value.emplace_back(val);
	}
	const_cast<QueueValue*>(this) -> cancel();
}

// ==============================================================

void QueueValue::clear()
{
	// Reset contents
	_value.clear();

	// Do nothing; we don't want to clobber the _value
	if (is_closed())
	{
		wait_and_take_all();
		return;
	}

	close();
	wait_and_take_all();
	open();
}

// ==============================================================

bool QueueValue::operator==(const Value& other) const
{
	// Derived classes use this, so use get_type()
	if (get_type() != other.get_type()) return false;

	if (this == &other) return true;

	if (not is_closed()) return false;
	if (not ((const QueueValue*) &other)->is_closed()) return false;

	return LinkValue::operator==(other);
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(QUEUE_VALUE,
                     createQueueValue, std::vector<ValuePtr>)
