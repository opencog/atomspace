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
	_decay = 30.0;
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
		_decay = nnp->get_value();
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

	// Keep distinct clocks for atoms and values.
	// That's because the first writer delays the second writer
	steady_clock::time_point atostart = steady_clock::now();
	steady_clock::time_point valstart = atostart;

	// After opening, sleep for the first fourth of the decay time.
	uint nappy = 1 + (uint) (1000.0 * _decay / 4.0);

	while(not _stop)
	{
		std::this_thread::sleep_for(milliseconds(nappy));

		steady_clock::time_point wake = steady_clock::now();
		if (not _atom_queue.is_empty())
		{
			// How long have we slept, in seconds?
			double waited = duration_cast<duration<double>>(wake-atostart).count();
			// What fraction of the decay time is that?
			double frac = waited / _decay;

			// How many Atoms awiting to be written?
			size_t qsz = _atom_queue.size();

			// How many should we write?
			double to_write = frac * ((double) qsz);
			uint nwrite = ceil(to_write);

			// Store that many
			for (int i=0; i < nwrite; i++)
			{
				Handle atom;
				bool got = _atom_queue.try_get(atom);
				if (not got) break;
				WriteThruProxy::storeAtom(atom);
			}
printf("duude wait=%f qsz=%lu nwrite=%lu\n", waited, qsz, nwrite);
		}
		atostart = wake;

		// re-measure, because above may have taken a long time.
		wake = steady_clock::now();

		// cut-n-paste of above.
		if (not _value_queue.is_empty())
		{
			// How long have we slept, in seconds?
			double waited = duration_cast<duration<double>>(wake-valstart).count();
			// What fraction of the decay time is that?
			double frac = waited / _decay;

			// How many Atoms awiting to be written?
			size_t qsz = _value_queue.size();

			// How many should we write?
			double to_write = frac * ((double) qsz);
			uint nwrite = ceil(to_write);

			// Store that many
			for (int i=0; i < nwrite; i++)
			{
				std::pair<Handle, Handle> kvp;
				bool got = _value_queue.try_get(kvp);
				if (not got) break;
				WriteThruProxy::storeValue(kvp.first, kvp.second);
			}
printf("duude vait=%f qsz=%lu nwrite=%lu\n", waited, qsz, nwrite);
		}
		valstart = wake;
	}
}

DEFINE_NODE_FACTORY(WriteBufferProxy, WRITE_BUFFER_PROXY_NODE)
