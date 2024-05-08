/*
 * WriteBufferProxy.cc
 *
 * Copyright (C) 2022 Linas Vepstas
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

#include <chrono>

#include <opencog/atoms/core/NumberNode.h>
#include <opencog/persist/proxy/WriteBufferProxy.h>

using namespace opencog;

// One writeback queue should be enough.
#define NUM_WB_QUEUES 1

WriteBufferProxy::WriteBufferProxy(const std::string&& name) :
	WriteThruProxy(WRITE_BUFFER_PROXY_NODE, std::move(name))
{
	init();
}

WriteBufferProxy::WriteBufferProxy(Type t, const std::string&& name) :
	WriteThruProxy(t, std::move(name))
{
	init();
}

WriteBufferProxy::~WriteBufferProxy()
{
}

void WriteBufferProxy::init(void)
{
	// Default decay time of 30 seconds
	_decay_ms = 30 * 1000;
	_stop = false;
}

// Get configuration from the ProxyParametersLink we live in.
void WriteBufferProxy::open(void)
{
	// Let ProxyNode::setup() do the basic work.
	WriteThruProxy::open();

	// Now fish out the rate parameter, if it is there.
	IncomingSet dli(getIncomingSetByType(PROXY_PARAMETERS_LINK));
	const Handle& pxy = dli[0];
	if (2 <= pxy->size())
	{
		const Handle& hdecay = pxy->getOutgoingAtom(2);
		if (not hdecay->is_type(NUMBER_NODE))
			throw SyntaxException(TRACE_INFO,
				"Expecting decay time in a NumberNode, got %s",
				hdecay->to_short_string().c_str());

		NumberNodePtr nnp = NumberNodeCast(hdecay);
		_decay_ms = (unsigned long int) (1000.0 * nnp->get_value());
	}

	// Start the writer
	_stop = false;
	_write_thread = std::thread(&WriteBufferProxy::write_loop, this);
}

void WriteBufferProxy::close(void)
{
	// Stop writing
	_stop = true;
	_write_thread.join();

	// Drain the queues
	barrier();
	_atom_queue.close();
	_value_queue.close();

printf("you close\n");
	WriteThruProxy::close();
}

void WriteBufferProxy::storeAtom(const Handle& h, bool synchronous)
{
	if (synchronous)
	{
		WriteThruProxy::storeAtom(h, synchronous);
		return;
	}
printf("yo store atom %s\n", h->to_string().c_str());
	_atom_queue.insert(h);
}

// Two-step remove. Just pass the two steps down to the children.
void WriteBufferProxy::preRemoveAtom(AtomSpace* as, const Handle& h,
                                     bool recursive)
{
	WriteThruProxy::preRemoveAtom(as, h, recursive);
}

void WriteBufferProxy::postRemoveAtom(AtomSpace* as, const Handle& h,
                                    bool recursive, bool extracted_ok)
{
	WriteThruProxy::postRemoveAtom(as, h, recursive, extracted_ok);
}

void WriteBufferProxy::storeValue(const Handle& atom, const Handle& key)
{
printf("yo store value %s\n", atom->to_string().c_str());
	_value_queue.insert({atom, key});
}

void WriteBufferProxy::updateValue(const Handle& atom, const Handle& key,
                            const ValuePtr& delta)
{
	WriteThruProxy::updateValue(atom, key, delta);
}

void WriteBufferProxy::barrier(AtomSpace* as)
{
	// Drain both queues.
	std::pair<Handle, Handle> pr;
	while (_value_queue.try_get(pr))
		WriteThruProxy::storeValue(pr.first, pr.second);

	Handle atom;
	while (_atom_queue.try_get(atom))
		WriteThruProxy::storeAtom(atom, false);

	WriteThruProxy::barrier(as);
}

// ==============================================================

// This runs in it's own thread, and drains a fraction of the queue.
void WriteBufferProxy::write_loop(void)
{
	using namespace std::chrono;

	steady_clock::time_point start = steady_clock::now();

	// After opening, sleep for the first fourth of the decay time.
	unsigned long int nappy = 1 + _decay_ms / 4;

	while(not _stop)
	{
		std::this_thread::sleep_for(milliseconds(nappy));
		if (not _atom_queue.is_empty())
		{
			size_t qsz = _atom_queue.size();
printf("duude qsz=%lu\n", qsz);
		}
	steady_clock::time_point wake = steady_clock::now();
	duration<double> time_span = duration_cast<duration<double>>(wake-start);
printf("duude write %f\n", time_span.count());
	}
}

DEFINE_NODE_FACTORY(WriteBufferProxy, WRITE_BUFFER_PROXY_NODE)
