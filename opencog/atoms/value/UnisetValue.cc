/*
 * opencog/atoms/value/UnisetValue.cc
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

#include <opencog/atoms/value/UnisetValue.h>
#include <opencog/atoms/value/VoidValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

// ==============================================================

UnisetValue::UnisetValue(const ValueSeq& vseq)
	: ContainerValue(UNISET_VALUE), _set(ValueComp(this))
{
	for (const ValuePtr& v: vseq)
		_set.insert(v);

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
void UnisetValue::update() const
{
	// Do nothing; we don't want to clobber the _value
	if (is_closed() and 0 == _set.size()) return;

	// Reset, to start with.
	_value.clear();

	// Loop forever, as long as the queue is open.
	try
	{
		while (true)
		{
			ValuePtr val;
			const_cast<UnisetValue*>(this)->_set.get(val);
			_value.emplace_back(val);
		}
	}
	catch (typename concurrent_set<ValuePtr, ValueComp>::Canceled& e)
	{}

	// If we are here, the queue closed up.
	// Drain any remaining values.
	ValueSeq rem(const_cast<UnisetValue*>(this)->_set.try_get(SIZE_MAX));
	_value.insert(_value.end(), rem.begin(), rem.end());
}

// ==============================================================

void UnisetValue::open()
{
	if (not is_closed()) return;
	_set.open();
}

void UnisetValue::close()
{
	if (is_closed()) return;
	_set.close();
}

bool UnisetValue::is_closed() const
{
	return _set.is_closed();
}

// ==============================================================

void UnisetValue::add(const ValuePtr& vp)
{
	_set.insert(vp);
}

void UnisetValue::add(ValuePtr&& vp)
{
	_set.insert(vp);
}

ValuePtr UnisetValue::remove(void)
{
	// Use try_get first, in case the set is closed.
	ValuePtr vp;
	if (_set.try_get(vp))
		return vp;

	// If we are here, then the set is empty.
	// If it is closed, then it's end-of-stream.
	// Else, we block and wait.
	// If it closes while we are blocked, we will catch an exception.
	// Return VoidValue as the end-of-stream marker.
	try
	{
		return _set.value_get();
	}
	catch (typename concurrent_set<ValuePtr, ValueComp>::Canceled& e)
	{}

	return createVoidValue();
}

size_t UnisetValue::size(void) const
{
	if (is_closed())
	{
		if (0 != _set.size()) update();
		return _value.size();
	}
	return _set.size();
}

// ==============================================================

void UnisetValue::clear()
{
	// Reset contents
	_value.clear();

	// Do nothing; we don't want to clobber the _value
	if (_set.is_closed())
	{
		_set.wait_and_take_all();
		return;
	}

	_set.close();
	_set.wait_and_take_all();
	_set.open();
}

// ==============================================================

bool UnisetValue::operator==(const Value& other) const
{
	if (this == &other) return true;

	if (not is_closed()) return false;
	if (other.is_type(UNISET_VALUE) and
	    not ((const UnisetValue*) &other)->is_closed()) return false;

	return LinkValue::operator==(other);
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(UNISET_VALUE,
                     createUnisetValue, std::vector<ValuePtr>)
