/*
 * opencog/atoms/parallel/ThreadJoinLink.cc
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
#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/parallel/ThreadJoinLink.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/value/LinkValue.h>

#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

ThreadJoinLink::ThreadJoinLink(const HandleSeq&& oset, Type t)
    : ParallelLink(std::move(oset), t)
{
}

static void thread_eval_tv(AtomSpace* as,
                           const Handle& evelnk, AtomSpace* scratch,
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

bool ThreadJoinLink::evaluate(AtomSpace* as,
                              bool silent,
                              AtomSpace* scratch)
{
	size_t arity = _outgoing.size();
	std::vector<TruthValuePtr> tvp(arity);

	// Create a collection of joinable threads.
	std::vector<std::thread> thread_set;
	std::exception_ptr ex;
	for (size_t i=0; i<arity; i++)
	{
		thread_set.push_back(std::thread(&thread_eval_tv,
			as, _outgoing[i], scratch, silent, &tvp[i], &ex));
	}

	// Wait for it all to come together.
	for (std::thread& t : thread_set) t.join();

	// Were there any exceptions? If so, rethrow.
	if (ex) std::rethrow_exception(ex);

	// Return the logical-AND of the returned truth values
	for (const TruthValuePtr& tv: tvp)
		if (0.5 > tv->get_mean()) return false;

	return true;
}

TruthValuePtr ThreadJoinLink::evaluate(AtomSpace* as,
                                       bool silent)
{
	bool ok = evaluate(as, silent, as);
	if (ok) SimpleTruthValue::TRUE_TV();
	return SimpleTruthValue::FALSE_TV();
}

DEFINE_LINK_FACTORY(ThreadJoinLink, THREAD_JOIN_LINK)
