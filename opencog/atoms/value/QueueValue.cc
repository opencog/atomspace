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
#include <opencog/atoms/value/VoidValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

typedef concurrent_queue<ValuePtr> conq;

// ==============================================================

QueueValue::QueueValue(const ValueSeq& vseq)
	: ContainerValue(QUEUE_VALUE)
{
	for (const ValuePtr& v: vseq)
		push(v); // concurrent_queue<ValuePtr>::push(v);

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
	if (is_closed() and 0 == conq::size()) return;

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
	catch (typename conq::Canceled& e)
	{}

	// If we are here, the queue closed up.
	// Drain any remaining values.
	std::queue<ValuePtr> rem =
		const_cast<QueueValue*>(this)->wait_and_take_all();

	_value.reserve(_value.size() + rem.size());
	while (not rem.empty())
	{
		_value.emplace_back(std::move(rem.front()));
		rem.pop();
	}
}

// ==============================================================

void QueueValue::open()
{
	if (not conq::is_closed()) return;
	conq::open();
}

void QueueValue::close()
{
	if (conq::is_closed()) return;
	conq::close();
}

bool QueueValue::is_closed() const
{
	return conq::is_closed();
}

// ==============================================================

void QueueValue::add(const ValuePtr& vp)
{
	conq::push(vp);
}

void QueueValue::add(ValuePtr&& vp)
{
	conq::push(vp);
}

ValuePtr QueueValue::remove(void)
{
	// Use try_get first, in case the queue is closed.
	ValuePtr vp;
	if (conq::try_get(vp))
		return vp;

	// If we are here, then the queue is empty.
	// If it is closed, then it's end-of-stream.
	// Else, we block and wait.
	// If the queue closes while we are blocked, we will catch an exception.
	// Return VoidValue as the end-of-stream marker.
	try
	{
		return conq::value_pop();
	}
	catch (typename conq::Canceled& e)
	{}
	return createVoidValue();
}

size_t QueueValue::size(void) const
{
	if (is_closed())
	{
		if (0 != conq::size()) update();
		return _value.size();
	}
	return conq::size();
}

// ==============================================================

void QueueValue::clear()
{
	// Reset contents
	_value.clear();

	// Do nothing; we don't want to clobber the _value
	if (conq::is_closed())
	{
		conq::wait_and_take_all();
		return;
	}

	conq::close();
	conq::wait_and_take_all();
	conq::open();
}

// ==============================================================

bool QueueValue::operator==(const Value& other) const
{
	if (this == &other) return true;

	if (not is_closed()) return false;
	if (other.is_type(QUEUE_VALUE) and
      not ((const QueueValue*) &other)->is_closed()) return false;

	return LinkValue::operator==(other);
}

std::string QueueValue::to_string(const std::string& indent) const
{
	// The default printer for QueueValue is LinkValue ...
	// with only one small problem: it will hang if the queue
	// is open. So we use it only if it is closed. Otherwise
	// we must punt. I mean, we could maybe print the contents
	// of an active queue, but this would be ... misleading,
	// as those contents would be changing even as the printer is
	// running. And would certanily be stale by the time the
	// print string is returned to the user.
	if (is_closed()) return LinkValue::to_string(indent);
	return indent + "(QueueValue) ;; currently open for writing";
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(QUEUE_VALUE,
                     createQueueValue, std::vector<ValuePtr>)
