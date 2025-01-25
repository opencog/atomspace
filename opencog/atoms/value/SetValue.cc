/*
 * opencog/atoms/value/SetValue.cc
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

#include <opencog/atoms/value/SetValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

typedef concurrent_set<ValuePtr> conset;

// ==============================================================

SetValue::SetValue(const ValueSeq& vseq)
	: ContainerValue(SET_VALUE)
{
	for (const ValuePtr& v: vseq)
		conset::insert(v);

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
void SetValue::update() const
{
	// Do nothing; we don't want to clobber the _value
	if (is_closed() and 0 == conset::size()) return;

	// Reset, to start with.
	_value.clear();

	// Loop forever, as long as the queue is open.
	try
	{
		while (true)
		{
			ValuePtr val;
			const_cast<SetValue*>(this) -> get(val);
			_value.emplace_back(val);
		}
	}
	catch (typename conset::Canceled& e)
	{}

	// If we are here, the queue closed up. Reopen it
	// just long enough to drain any remaining values.
	const_cast<SetValue*>(this) -> cancel_reset();
	while (not is_empty())
	{
		ValuePtr val;
		const_cast<SetValue*>(this) -> get(val);
		_value.emplace_back(val);
	}
	const_cast<SetValue*>(this) -> cancel();
}

// ==============================================================

void SetValue::open()
{
	if (not is_closed()) return;
	conset::open();
}

void SetValue::close()
{
	if (is_closed()) return;
	conset::close();
}

bool SetValue::is_closed() const
{
	return conset::is_closed();
}

// ==============================================================

void SetValue::add(const ValuePtr& vp)
{
	conset::insert(vp);
}

void SetValue::add(ValuePtr&& vp)
{
	conset::insert(vp);
}

ValuePtr SetValue::remove(void)
{
	return conset::value_get();
}

size_t SetValue::size(void) const
{
	return conset::size();
}

// ==============================================================

void SetValue::clear()
{
	// Reset contents
	_value.clear();

	// Do nothing; we don't want to clobber the _value
	if (conset::is_closed())
	{
		conset::wait_and_take_all();
		return;
	}

	conset::close();
	conset::wait_and_take_all();
	conset::open();
}

// ==============================================================

bool SetValue::operator==(const Value& other) const
{
	// Derived classes use this, so use get_type()
	if (get_type() != other.get_type()) return false;

	if (this == &other) return true;

	if (not is_closed()) return false;
	if (not ((const SetValue*) &other)->is_closed()) return false;

	return LinkValue::operator==(other);
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(SET_VALUE,
                     createSetValue, std::vector<ValuePtr>)
