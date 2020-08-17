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

#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/parallel/ExecuteThreadedLink.h>
#include <opencog/atoms/value/QueueValue.h>

#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

ExecuteThreadedLink::ExecuteThreadedLink(const HandleSeq&& oset, Type t)
    : Link(std::move(oset), t), _nthreads(-1)
{
	if (0 == _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"Expecting at least one argument!");

	Type nt = _outgoing[0]->get_type();
	if (NUMBER_NODE == nt)
		_nthreads = std::floor(NumberNodeCast(_outgoing[0])->get_value());
}

static void thread_eval_tv(AtomSpace* as,
                           const Handle& evelnk,
                           bool silent, TruthValuePtr* tv,
                           std::exception_ptr* returned_ex)
{
	try
	{
		*tv = EvaluationLink::do_eval_scratch(as, evelnk, scratch, silent);
	}
	catch (const std::exception& ex)
	{
		*returned_ex = std::current_exception();
	}
}

ValuePtr ExecuteThreadedLink::execute(AtomSpace* as,
                                      bool silent)
{
	size_t arity = _outgoing.size();
	std::vector<ValuePtr> tvp(arity);

	// Create a collection of joinable threads.
	std::vector<std::thread> thread_set;
	std::exception_ptr ex;
	for (size_t i=0; i<arity; i++)
	{
		thread_set.push_back(std::thread(&thread_eval_tv,
			as, oset[i], silent, &tvp[i], &ex));
	}

	// Wait for it all to come together.
	for (std::thread& t : thread_set) t.join();

	// Were there any exceptions? If so, rethrow.
	if (ex) std::rethrow_exception(ex);
}

DEFINE_LINK_FACTORY(ExecuteThreadedLink, EXECUTE_THREADED_LINK)
