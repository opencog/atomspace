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
	// Default decay time of 60 seconds
	_decay = 60.0;
#define HIMAX 64123123   // approx 4GBytes
	_high_water_mark = HIMAX;
	_stop = false;
	reset_stats();
}

// Get configuration from the ProxyParametersLink we live in.
void WriteBufferProxy::open(void)
{
	// Let ProxyNode::setup() do the basic work.
	WriteThruProxy::open();

	// Now fish out the time decay const, if it is there.
	IncomingSet dli(getIncomingSetByType(PROXY_PARAMETERS_LINK));
	const Handle& pxy = dli[0];
	if (2 < pxy->size())
	{
		const Handle& hdecay = pxy->getOutgoingAtom(2);
		if (not hdecay->is_type(NUMBER_NODE))
			throw SyntaxException(TRACE_INFO,
				"Expecting decay time in a NumberNode, got %s",
				hdecay->to_short_string().c_str());

		NumberNodePtr nnp = NumberNodeCast(hdecay);
		_decay = nnp->get_value();
	}

	// Reset the high-water mark.
	_high_water_mark = HIMAX;

	// Remove all previous pending writes (if any).
	// This can happen if this was open for reading previously,
	// and some crazy error was thrown, and somehow the buffer
	// was closed before the writes flushed. Not sure how this
	// can happen, but safety first ...
	_atom_queue.clear();
	_value_queue.clear();

	// Open the queues
	_atom_queue.open();
	_value_queue.open();

	// Start the writer.
	_stop = false;
	_write_thread = std::thread(&WriteBufferProxy::write_loop, this);
}

void WriteBufferProxy::close(void)
{
	// Stop writing. The thread may be sleeping, so the join() might
	// hang for a fraction of _decay seconds. It does not seem worthwhile
	// to try to speed this up, e.g. with a condition variable.
	_stop = true;
	_write_thread.join();

	// Drain the queues
	barrier();
	_atom_queue.close();
	_value_queue.close();

	WriteThruProxy::close();
}

void WriteBufferProxy::storeAtom(const Handle& h, bool synchronous)
{
	if (synchronous)
	{
		WriteThruProxy::storeAtom(h, synchronous);
		return;
	}
	_atom_queue.insert(h);
	_astore ++;

	// Stall if oversize
	if (_high_water_mark < _atom_queue.size())
		sleep(_ticker);
}

// Two-step remove. Just pass the two steps down to the children.
void WriteBufferProxy::preRemoveAtom(AtomSpace* as, const Handle& h,
                                     bool recursive)
{
	WriteThruProxy::preRemoveAtom(as, h, recursive);
}

void WriteBufferProxy::erase_recursive(const Handle& h)
{
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
		_atom_queue.erase(h);
	}

	// There is no effective way of doing a remove from the value queue,
	// because we don't have the associated key. (We could try looping,
	// but that would be a huge waste of CPU). So instead, just flush
	// the queue. We could ask the user to call barrier(), but the user
	// might forget, so we do it for them.
	if (not _value_queue.is_empty())
	{
		std::pair<Handle, Handle> pr;
		while (_value_queue.try_get(pr))
			WriteThruProxy::storeValue(pr.first, pr.second);

		WriteThruProxy::barrier();
	}

	WriteThruProxy::postRemoveAtom(as, h, recursive, extracted_ok);
}

void WriteBufferProxy::storeValue(const Handle& atom, const Handle& key)
{
	_value_queue.insert({atom, key});
	_vstore ++;

	// Stall if oversize
	if (_high_water_mark < _value_queue.size())
		sleep(_ticker);
}

void WriteBufferProxy::updateValue(const Handle& atom, const Handle& key,
                                   const ValuePtr& delta)
{
	// XXX FIXME. Buffering these naively, like this, voilates the
	// intent of how this method should work. However, for the
	// RocksStorageNode, doing this is harmless. And the
	// CogStorageNode is just a pass-through. So there are no
	// existing StorageNodes that actually depend on delta.
	// (The Value at key has already been atomically incremented,
	// by the time we get here.) So just buffer these like regular
	// storeValue() calls. This may result in incorrect behavior
	// in some futuristic scenario that involves StorageNodes that
	// don't yet exist. But I'm not gonna write complicated code to
	// handle some hypothetical future case that may never exist.
	storeValue(atom, key);
}

void WriteBufferProxy::barrier(AtomSpace* as)
{
	_nbars ++;

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

void WriteBufferProxy::reset_stats(void)
{
	_nstalls = 0;
	_nbars = 0;
	_ndumps = 0;
	_astore = 0;
	_vstore = 0;
	_mavg_in_atoms = 0.0;
	_mavg_in_values = 0.0;
	_mavg_buf_atoms = 0.0;
	_mavg_buf_values = 0.0;
	_mavg_out_atoms = 0.0;
	_mavg_out_values = 0.0;
	_mavg_load = 0.0;
}

std::string WriteBufferProxy::monitor(void)
{
	std::string rpt;
	rpt += "Write Buffer Proxy: ";
	rpt += "writes: " + std::to_string(_ndumps);
	rpt += "   barriers: " + std::to_string(_nbars);
	rpt += "   stalls: " + std::to_string(_nstalls);
	rpt += "\n";

	// std::to_string prints six decimal places but we want zero.
#define PFLO(X) std::to_string((int)round(X))
	rpt += "Avg inflow, Atoms: " + PFLO(_mavg_in_atoms);
	rpt += "    Values: " + PFLO(_mavg_in_values);
	rpt += "\n";

	rpt += "Avg buffer size, Atoms: " + PFLO(_mavg_buf_atoms);
	rpt += "    Values: " + PFLO(_mavg_buf_values);
	rpt += "\n";

	rpt += "Avg outflow, Atoms: " + PFLO(_mavg_out_atoms);
	rpt += "    Values: " + PFLO(_mavg_out_values);
	rpt += "\n";

	// Duty cycle is the amount of time that the write thread
	// is actually writing, vs. the elapsed wallclock time.
	// Anything over 100 will lead to buffer overflows.
	rpt += "Timescale " + PFLO(_decay) + " secs;  ";
	rpt += "Duty cycle (load avg): " + std::to_string(_mavg_load);
	rpt += "\n";

	return rpt;
}

// ==============================================================

// This runs in it's own thread, and drains a fraction of the queue.
// Currently, only one thread is used for draining the queue. The
// assumption is that additional threads will not help, because of
// lock contention in the target. But I don't know for sure. Indirect
// evidence from RocksStorage indicates that bombarding it from multiple
// threads does not improve throughput. So, for now, just one thread.
void WriteBufferProxy::write_loop(void)
{
	// Keep a moving average queue size. This is used to determine
	// when the queue is almost empty, by historical standards, which
	// is then used to flsuh out the remainer of the queue.
	reset_stats();

	using namespace std::chrono;

	_ticker = 0.25 * _decay;
	if (10.0 < _ticker) _ticker = 10.0;

	// Set amount to write per cycle.
	double frac = _ticker / _decay;

	// After opening, sleep for a little while
	uint nappy = 1 + ceil(1000.0 * _ticker);
	std::this_thread::sleep_for(milliseconds(nappy));

	// Start with non-zero moving avg, approximating what it should be.
	_mavg_in_atoms = _astore;
	_mavg_in_values = _vstore;
	_mavg_buf_atoms = (double) _atom_queue.size();
	_mavg_buf_values = (double) _value_queue.size();
	_mavg_out_atoms = frac * _mavg_buf_atoms;
	_mavg_out_values = frac * _mavg_buf_values;

	while (not _stop)
	{
		steady_clock::time_point awake = steady_clock::now();

		bool wrote = false;
		if (not _atom_queue.is_empty())
		{
			wrote = true;

			// How many Atoms waiting to be written?
			double qsz = (double) _atom_queue.size();

			// How many should we write?
			uint nwrite = ceil(frac * qsz);

			// Moving average of the last ten writes. Is that OK?
#define WEI 0.1
			_mavg_buf_atoms = (1.0-WEI) * _mavg_buf_atoms + WEI * qsz;

			// Whats the min to write? The goal here is to not
			// dribble out the tail, but to push it out, if its
			// almost all gone anyway.
			uint mwr = ceil(0.5 * frac * _mavg_buf_atoms);
			if (mwr < 1000) mwr = 1000;
			if (nwrite < mwr) nwrite = mwr;

			// Store that many
			HandleSeq avec = _atom_queue.try_get(nwrite, 0 == nwrite%7);
			for (const Handle& h : avec)
				WriteThruProxy::storeAtom(h);

			// Collect performance stats
			_mavg_in_atoms = (1.0-WEI) * _mavg_in_atoms + WEI * _astore;
			_astore = 0;
			_mavg_out_atoms = (1.0-WEI) * _mavg_out_atoms + WEI * avec.size();
		}

		// Cut-n-paste of above.
		if (not _value_queue.is_empty())
		{
			wrote = true;

			// How many values are waiting to be written?
			double qsz = (double) _value_queue.size();

			// How many should we write?
			uint nwrite = ceil(frac * qsz);

			// Moving avg
			_mavg_buf_values = (1.0-WEI) * _mavg_buf_values + WEI * qsz;

			// Min to write.
			uint mwr = ceil(0.5 * frac * _mavg_buf_values);
			if (nwrite < mwr) nwrite = mwr;

			// Store that many
			std::vector<std::pair<Handle, Handle>> vav =
				_value_queue.try_get(nwrite, 0 == nwrite%7);
			for (const std::pair<Handle, Handle>& kvp : vav)
				WriteThruProxy::storeValue(kvp.first, kvp.second);

			// Collect performance stats
			_mavg_in_values = (1.0-WEI) * _mavg_in_values + WEI * _vstore;
			_vstore = 0;
			_mavg_out_values = (1.0-WEI) * _mavg_out_values + WEI * vav.size();
		}
		if (wrote) _ndumps ++;

		// How much time have we used up so far?
		steady_clock::time_point wrdone = steady_clock::now();
		double wrtime = duration_cast<duration<double>>(wrdone-awake).count();

		// Moving averge duty factor.
		_mavg_load = (1.0-WEI) * _mavg_load + WEI * wrtime / _ticker;

		// How much time do we have left to sleep?
		double left = _ticker - wrtime;
		if (0.0 < left)
		{
			nappy = floor(1000.0 * left);

			// I guess we're good. Relax the high-water mark a bit.
			if (_high_water_mark < HIMAX)
			{
				_high_water_mark *= 17;
				_high_water_mark /= 16;
			}
			std::this_thread::sleep_for(milliseconds(nappy));
		}
		else
		{
			// Oh no! Cannot keep up with the requested time limit!
			// Pause readers until the queue drains down a bit.
			// _mavg_out is how many we are able to actually write,
			// per "used" interval. Scaling sets a buffer size that
			// should be able to clear at exactly that rate. So this
			// should be able to hold the duty cycle to #define DUTY
			// as the mean value. By setting DUTY_CYCLE to greater
			// than one, this will keep the write rate saturated
			// at max possible, so that we are *always* stalling.
			// And that's OK, because the _high_water_mark will
			// rate limit the inflow. And the average buffer size
			// will then be ("exactly") equal to _high_water_mark.
			// The only thing that _decay does is to increase the
			// high-water mark. Other than that, the system runs
			// maxed out.  An alternative API would allow the user
			// to set the watermark directly, and we reverse all
			// these calculations to get a _decay value that's
			// suitable. Right? Same thing in the end ...
			nappy = 0;
			left = 0.0;
			double worst = fmax(_mavg_out_atoms, _mavg_out_values);
#define DUTY_CYCLE 1.2
			_high_water_mark = DUTY_CYCLE * worst * _decay / wrtime;
			_nstalls ++;
		}
	}
}

DEFINE_NODE_FACTORY(WriteBufferProxy, WRITE_BUFFER_PROXY_NODE)
