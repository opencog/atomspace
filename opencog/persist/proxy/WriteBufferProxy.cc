/*
 * WriteBufferProxy.cc
 *
 * Copyright (C) 2024 Linas Vepstas
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
#include <math.h>

#include <opencog/atoms/core/NumberNode.h>
#include <opencog/persist/proxy/WriteBufferProxy.h>

using namespace opencog;

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

	// Start the writer.
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

printf("duuuuude close\n");
	WriteThruProxy::close();
}

// It can happen that, if WriteThruProxy::storeAtom() is really slow,
// that the buffering will outrun the real writer. Which will be bad.
// So hard-code a max size of a million; if this is exceeded, stall
// until the queue drains. A million atoms are a bit under 1GB RAM,
// depending, so this should cover present-day systems.
#define MAX_QUEUE_SIZE 1000123

void WriteBufferProxy::storeAtom(const Handle& h, bool synchronous)
{
	if (synchronous)
	{
		WriteThruProxy::storeAtom(h, synchronous);
		return;
	}
	_atom_queue.insert(h);

	// Stall if oversize
	if (MAX_QUEUE_SIZE < _atom_queue.size())
	{
		while ((MAX_QUEUE_SIZE/4) < _atom_queue.size())
			sleep(_decay);
	}
}

// Two-step remove. Just pass the two steps down to the children.
void WriteBufferProxy::preRemoveAtom(AtomSpace* as, const Handle& h,
                                     bool recursive)
{
	WriteThruProxy::preRemoveAtom(as, h, recursive);
}

void WriteBufferProxy::erase_recursive(const Handle& h)
{
	// _value_queue.erase(h);
	_atom_queue.erase(h);
	IncomingSet ris(h->getIncomingSet());
	for (const Handle& hi : ris)
		erase_recursive(hi);
}

void WriteBufferProxy::postRemoveAtom(AtomSpace* as, const Handle& h,
                                      bool recursive, bool extracted_ok)
{
	if (recursive)
		erase_recursive(h);
	else
	{
		// _value_queue.erase(h);
		_atom_queue.erase(h);
	}
	WriteThruProxy::postRemoveAtom(as, h, recursive, extracted_ok);
}

void WriteBufferProxy::storeValue(const Handle& atom, const Handle& key)
{
	_value_queue.insert({atom, key});

	// Stall if oversize
	if (MAX_QUEUE_SIZE < _value_queue.size())
	{
		while ((MAX_QUEUE_SIZE/4) < _value_queue.size())
			sleep(_decay);
	}
}

void WriteBufferProxy::updateValue(const Handle& atom, const Handle& key,
                                   const ValuePtr& delta)
{
// XXX FIXME We can buffer these, if we do an atom increment
// of what's in the queue.
	WriteThruProxy::updateValue(atom, key, delta);
}

void WriteBufferProxy::barrier(AtomSpace* as)
{
	// Unconditionally drain both queues.
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

	// Keep a moving average queue size. This is used to determine
	// when the queue is almost empty, by historical standards, which
	// is then used to flsuh out the remainer of the queue.
	double mavg_atoms = 0.0;
	double mavg_vals = 0.0;

#define POLLY 4.0
	// POLLY=4, minfrac = 1-exp(-0.25) = 1-0.7788 = 0.2212;
	// This is used to set a minimum write value, at half of this.
	static const double minfrac = 1.0 - exp(-1.0/POLLY);

	// After opening, sleep for the first fourth of the decay time.
	double ticker = _decay / POLLY;
	uint nappy = 1 + ceil(1000.0 * ticker);

	while(not _stop)
	{
		if (0 < nappy) std::this_thread::sleep_for(milliseconds(nappy));

		steady_clock::time_point awake = steady_clock::now();
		if (not _atom_queue.is_empty())
		{
			// How long have we slept, in seconds?
			double waited = duration_cast<duration<double>>(awake-atostart).count();
			// What fraction of the decay time is that?
			double frac = waited / _decay;

			// How many Atoms awaiting to be written?
			double qsz = (double) _atom_queue.size();
#define WEI (0.3 / POLLY)
			mavg_atoms = (1.0-WEI) * mavg_atoms + WEI * qsz;

			// How many should we write?
			uint nwrite = ceil(frac * qsz);

			// Whats the min to write? The goal here is to not
			// dribble out the tail, but to push it out, if its
			// almost all gone anyway.
			uint mwr = ceil(0.5 * minfrac * mavg_atoms);
			if (nwrite < mwr) nwrite = mwr;

			// Store that many
			for (uint i=0; i < nwrite; i++)
			{
				Handle atom;
				bool got = _atom_queue.try_get(atom);
				if (not got) break;
				WriteThruProxy::storeAtom(atom);
			}
		}
		atostart = awake;

		// re-measure, because above may have taken a long time.
		steady_clock::time_point vwake = steady_clock::now();

		// cut-n-paste of above.
		if (not _value_queue.is_empty())
		{
			// How long have we slept, in seconds?
			double waited = duration_cast<duration<double>>(vwake-valstart).count();
			// What fraction of the decay time is that?
			double frac = waited / _decay;

			// How many values are waiting to be written?
			double qsz = (double) _value_queue.size();
			mavg_vals = (1.0-WEI) * mavg_vals + WEI * qsz;

			// How many should we write?
			uint nwrite = ceil(frac * qsz);

			// Min to write.
			uint mwr = ceil(0.5 * minfrac * mavg_vals);
			if (nwrite < mwr) nwrite = mwr;

			// Store that many
			for (uint i=0; i < nwrite; i++)
			{
				std::pair<Handle, Handle> kvp;
				bool got = _value_queue.try_get(kvp);
				if (not got) break;
				WriteThruProxy::storeValue(kvp.first, kvp.second);
			}
		}
		valstart = vwake;

		// How much time have we used up so far?
		steady_clock::time_point elap = steady_clock::now();
		double used = duration_cast<duration<double>>(elap-awake).count();
		// How much time do we have left to sleep?
		double left = ticker - used;
		if (0.0 > left) left = 0.0;
		nappy = floor(1000.0 * left);
	}
}

DEFINE_NODE_FACTORY(WriteBufferProxy, WRITE_BUFFER_PROXY_NODE)
