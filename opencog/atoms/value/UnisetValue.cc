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
#include <opencog/atoms/base/Link.h>

using namespace opencog;

// ==============================================================

UnisetValue::UnisetValue(const ValueSeq& vseq)
	: ContainerValue(UNISET_VALUE), _set(ValueComp(this)), _source(nullptr)
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
	// Grab whatever we can from upstream.
	drain();

#if 1
	// XXX FIXME ... I don't know if this is correct. It might be.
	// At any rate, no unit test expects this.
	//
	// The blocking semantics means that after the set closes,
	// everything is copied from _set to the local _value.
	// Thus, removals cannot come from _set any longer, they
	// must come from the local _value.
	if (is_closed())
	{
		update();
		if (0 == size())
			return createVoidValue();

		auto front = _value.begin();
		ValuePtr vp = *front;
		_value.erase(front);
		return vp;
	}
#endif

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

/// Return one item from the set, without removing it.
/// Returns nullptr if the set is empty.
ValuePtr UnisetValue::peek(void) const
{
	auto item = _set.peek();
	if (not item.has_value()) return nullptr;
	return item.value();
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

// Set up all finite streams here. Finite streams get copied into the
// collection once and once only, and that's it. The copy is done one
// at a time, so that the set relation can be applied as each item is
// inserted.
void UnisetValue::init_src(const ValuePtr& src)
{
	// Copy Link contents into the collection.
	if (src->is_type(LINK))
	{
		for (const Handle& h: HandleCast(src)->getOutgoingSet())
			_set.insert(h);
		_set.close();
		return;
	}

	// Everything else is just a collection of size one.
	// Possible future extensions:
	// If _source is an ObjectNode, then send *-read-* message ???
	// If source is a FloatStream or StringStream ... ???
	if (not src->is_type(LINK_VALUE))
	{
		_set.insert(src);
		_set.close();
		return;
	}

	// One-shot, non-streaming finite LinkValue
	if (not src->is_type(CONTAINER_VALUE) and
	    not src->is_type(STREAMING_SIG) and
	    not src->is_type(HOARDING_SIG))
	{
		ValueSeq vsq = LinkValueCast(src)->value();
		for (const ValuePtr& vp: vsq)
			_set.insert(vp);
		_set.close();
		return;
	}

	// If it's a container, and its closed, then its a finite,
	// one-shot deal, just like the above.
	if (src->is_type(CONTAINER_VALUE) and
	    ContainerValueCast(src)->is_closed())
	{
		ValueSeq vsq = LinkValueCast(src)->value();
		for (const ValuePtr& vp: vsq)
			_set.insert(vp);
		_set.close();
		return;
	}

	// If we are here, then the data source is either a StreamingSig
	// or a HoardingSig. These are potentially infinite sources,
	// they typically block during reading; so we cannot handle them
	// here. Instead, these will be polled later, by drain().
	_source = LinkValueCast(src);
}

void UnisetValue::drain(void) const
{
	if (nullptr == _source) return;

	// Plain streams are easy. Just sample and go.
	if (not _source->is_type(CONTAINER_VALUE))
	{
		// Use source size() as a surrogate to tell us if the source
		// will block. Pull as much as we can, without blocking.
		while (0 < _source->size())
		{
			ValueSeq vsq = _source->value();

			// Zero-sized sequences (e.g. VoidValue) indicate end-of-stream.
			if (0 == vsq.size())
			{
				_set.close();
				return;
			}

			// !!??? We flatten here. Is this correct?
			for (const ValuePtr& vp: vsq)
				_set.insert(vp);
		}
		return;
	}

	// If we are here, we've got a container ... It needs to be drained
	// one at a time.
	ContainerValuePtr cvp = ContainerValueCast(_source);
	while (0 < cvp->size() or cvp->is_closed())
	{
#if IS_THIS_NEEDED
		ValuePtr vp;
		try
		{
			vp = cvp->remove();
		}
		catch (typename concurrent_set<ValuePtr, ValueComp>::Canceled& e)
		{
			_set.close(); // We are done; close shop.
			return;
		}
#endif

		ValuePtr vp = cvp->remove();

		// Zero-sized sequences (e.g. VoidValue) indicate end-of-stream.
		if (0 == vp->size())
		{
			if (not _set.is_closed())
				_set.close();
			return;
		}

		_set.insert(vp);
	}
}

// ==============================================================

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(UNISET_VALUE,
                     createUnisetValue, std::vector<ValuePtr>)
