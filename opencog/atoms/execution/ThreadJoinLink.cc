/*
 * opencog/atoms/execution/ThreadJoinLink.cc
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
#include <opencog/atoms/execution/ThreadJoinLink.h>
#include <opencog/atoms/value/LinkValue.h>

#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

ThreadJoinLink::ThreadJoinLink(const HandleSeq&& oset, Type t)
    : Link(std::move(oset), t)
{
}

static void thread_eval(AtomSpace* as,
                        const Handle& evelnk, AtomSpace* scratch,
                        bool silent)
{
	try
	{
		ThreadJoinLink::do_eval_scratch(as, evelnk, scratch, silent);
	}
	catch (const std::exception& ex)
	{
		logger().warn("Caught exception in thread:\n%s", ex.what());
	}
}

ValuePtr ThreadJoinLink::execute(AtomSpace* as,
                               bool silent,
                               AtomSpace* scratch)
{
	// Create and detach threads; return immediately.
	for (const Handle& h : getOutgoingSet())
	{
		std::thread thr(&thread_eval, as, h, scratch, silent);
		thr.detach();
	}
	return SimpleTruthValue::TRUE_TV();
}

DEFINE_LINK_FACTORY(ThreadJoinLink, THREAD_JOIN_LINK)
