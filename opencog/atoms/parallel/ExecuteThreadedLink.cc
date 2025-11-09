/*
 * opencog/atoms/parallel/ExecuteThreadedLink.cc
 *
 * Copyright (C) 2009, 2013, 2014, 2015, 2020 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
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

#include <cmath>
#include <thread>

#include <opencog/util/platform.h>
#include <opencog/util/concurrent_queue.h>

#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/parallel/ExecuteThreadedLink.h>
#include <opencog/atoms/value/QueueValue.h>

#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

/// ExecuteThreadedLink
/// Perform execution in parallel threads.
/// The general structure of this link is
///
///        ExecuteThreadedLink
///            NumberNode nthr  ; optional; if present, number of threads.
///            SetLink
///                ExecutableAtoms...
///
/// or
///
///        ExecuteThreadedLink
///            NumberNode nthr  ; optional; if present, number of threads.
///            ExecutableAtoms...
///
/// When this link is executed, the `ExecutableAtoms...` are executed
/// in parallel, in distinct threads, with the result of the execution
/// appended to a thread-safe queue, the QueueValue.  The QueueValue is
/// returned immediately. The QueueValue remains open as long as threads
/// are running; it is closed only after all threads terminate.
///
/// By default, the number of threads launched equals the number of
/// Atoms in the set. If the NumberNode is present, then the number of
/// threads is the smaller of the NumberNode and the size of the Set.
///
/// XXX TODO: If nthreads = 0, just execute directly here, in the
/// current thread, and block till execution is done.
///
/// XXX TODO: Create some kind of thing that waits for the QueueValue
/// to close. This would have the effect of joining.  For example:
///    (cog-execute! (WaitForCloseLink (ExecuteThreadedLink ...)))
/// and the WaitForCloseLink just ... waits for the queue to close,
/// and returns only then. This would be generic, for all QueueValue
/// users... XXX should port QueryLink etc. to this, too!?

ExecuteThreadedLink::ExecuteThreadedLink(const HandleSeq&& oset, Type t)
    : Link(std::move(oset), t), _nthreads(-1)
{
	if (0 == _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"Expecting at least one argument!");

	size_t off = 0;
	if (_outgoing[0]->is_type(NUMBER_NODE))
	{
		if (1 == _outgoing.size())
			throw InvalidParamException(TRACE_INFO,
				"Expecting a set of executable links!");
		_nthreads = std::floor(NumberNodeCast(_outgoing[0])->get_value());
		off = 1;
	}

	size_t nitems = 0;
	for (const Handle& h: _outgoing)
		if (SET_LINK == h->get_type()) nitems += h->get_arity();

	if (0 == nitems) nitems = _outgoing.size() - off;

	_nthreads = std::min(_nthreads, nitems);
}

ExecuteThreadedLink::~ExecuteThreadedLink()
{
	// Must make sure the memory for the thread status outlives
	// the thread it's tracking; otherwise we risk freeing the
	// RAM for _joiner, after which the joiner thread will write
	// to it. As long as this Atom is in an AtomSpace, this dtor
	// will never run, but if the AtomSpace is being destroyed,
	// then bad things happen without this. e.g. ThreadedUTest.
	if (_joiner.joinable())
		_joiner.join();
}

static void thread_exec(AtomSpace* as, bool silent,
                        concurrent_queue<Handle>* todo,
                        QueueValuePtr qvp,
                        std::exception_ptr* returned_ex)
{
	set_thread_name("atoms:execlink");
	while (true)
	{
		Handle h;
		if (not todo->try_get(h)) return;

		// This is (supposed to be) identical to what cog-execute!
		// would do...
		Instantiator inst(as);
		try
		{
			ValuePtr pap(inst.execute(h));
			if (pap and pap->is_atom())
				pap = as->add_atom(HandleCast(pap));
			qvp->add(std::move(pap));
		}
		catch (const std::exception& ex)
		{
			*returned_ex = std::current_exception();
			return;
		}
	}
}

static void thread_joiner(AtomSpace* as, bool silent,
                          Handle exlnk,
                          size_t nthreads,
                          QueueValuePtr qvp)
{
	// Place the work items onto a queue.
	concurrent_queue<Handle> todo_list;
	bool chk = true;
	for (const Handle& h: exlnk->getOutgoingSet())
	{
		if (chk and h->is_type(NUMBER_NODE)) continue;
		chk = false;

		if (not (SET_LINK == h->get_type()))
		{
			todo_list.push(h);
			continue;
		}

		// Unwrap SetLinks. This kind of unwrapping is historical
		// baggage, coming from the old BindLink implementation.
		// I suspect that special-casing for SetLinks should be
		// removed someday. Just not today.
		for (const Handle& hs: h->getOutgoingSet())
			todo_list.push(hs);
	}

	std::exception_ptr ex;

	// Launch the workers
	std::vector<std::thread> thread_set;
	for (size_t i=0; i<nthreads; i++)
	{
		thread_set.push_back(std::thread(&thread_exec,
			as, silent, &todo_list, qvp, &ex));
	}

	// Wait for all threads to come together.
	for (std::thread& t : thread_set) t.join();

#if 0
	// Were there any exceptions? If so, rethrow.
	// XXX Wait, how ???
	if (ex) std::rethrow_exception(ex);
#endif

	qvp->close();
}

ValuePtr ExecuteThreadedLink::execute(AtomSpace* as,
                                      bool silent)
{
	// If a previous invocation is still running, wait for it
	// to finish. This avoids memory management issues.
	if (_joiner.joinable())
		_joiner.join();

	QueueValuePtr qvp(createQueueValue());

	std::thread jnr(&thread_joiner,
		as, silent, get_handle(), _nthreads, qvp);
	_joiner.swap(jnr);

	return qvp;
}

DEFINE_LINK_FACTORY(ExecuteThreadedLink, EXECUTE_THREADED_LINK)
