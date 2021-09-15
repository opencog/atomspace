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
///            NumberNode nthr  ; optionl; if present, number of threads.
///            SetLink
///                ExecutableAtoms...
///
/// When this link is executed, the `ExecutableAtoms...` are executed
/// in parallel, in distinct threads, with the result of the execution
/// appended to a thread-safe queue, the QueueValue.  After all of the
/// atoms have been executed, the QueueValue holding the results is
/// returned. Execution blocks until all of the threads have finished.
///
/// By default, the number of threads launched equals the number of
/// Atoms in the set. If the NumberNode is present, then the number of
/// threads is the smaller of the NumberNode and the seize of the Set.
///
/// XXX TODO: We could have a non-blocking version of this atom. We
/// could just return the QueueValue immediately; the user could check
/// to see if the queue is closed, to find out if the threads have
/// finished.

ExecuteThreadedLink::ExecuteThreadedLink(const HandleSeq&& oset, Type t)
    : Link(std::move(oset), t), _nthreads(-1), _setoff(0)
{
	if (0 == _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"Expecting at least one argument!");

	Type nt = _outgoing[0]->get_type();
	Type st = nt;
	if (NUMBER_NODE == nt)
	{
		if (1 == _outgoing.size())
			throw InvalidParamException(TRACE_INFO,
				"Expecting a set of executable links!");
		_nthreads = std::floor(NumberNodeCast(_outgoing[0])->get_value());

		// The set link.
		_setoff = 1;
		st = _outgoing[1]->get_type();
	}

	if (SET_LINK != st)
		throw InvalidParamException(TRACE_INFO,
			"Expecting a set of executable links!");

	_nthreads = std::min(_nthreads, _outgoing[_setoff]->get_arity());
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

		// This is "identical" to what cog-execute! would do...
		Instantiator inst(as);
		try
		{
			ValuePtr pap(inst.execute(h));
			if (pap and pap->is_atom())
				pap = as->add_atom(HandleCast(pap));
			qvp->push(std::move(pap));
		}
		catch (const std::exception& ex)
		{
			*returned_ex = std::current_exception();
			return;
		}
	}
}

ValuePtr ExecuteThreadedLink::execute(AtomSpace* as,
                                      bool silent)
{
	// Place the work items onto a queue.
	concurrent_queue<Handle> todo_list;
	const HandleSeq& exes = _outgoing[_setoff]->getOutgoingSet();
	for (const Handle& h: exes)
		todo_list.push(h);

	// Where the results will be reported.
	QueueValuePtr qvp(createQueueValue());

	// Create a collection of joinable threads.
	std::vector<std::thread> thread_set;
	std::exception_ptr ex;

	// Launch the workers
	for (size_t i=0; i<_nthreads; i++)
	{
		thread_set.push_back(std::thread(&thread_exec,
			as, silent, &todo_list, qvp, &ex));
	}

	// Wait for it all to come together.
	for (std::thread& t : thread_set) t.join();

	// Were there any exceptions? If so, rethrow.
	if (ex) std::rethrow_exception(ex);

	qvp->close();
	return qvp;
}

DEFINE_LINK_FACTORY(ExecuteThreadedLink, EXECUTE_THREADED_LINK)
