/*
 * opencog/atoms/parallel/ParallelLink.cc
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

#include <sys/prctl.h>
#include <thread>

#include <opencog/atoms/execution/EvaluationLink.h>
#include <opencog/atoms/parallel/ParallelLink.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>

#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

ParallelLink::ParallelLink(const HandleSeq&& oset, Type t)
    : UnorderedLink(std::move(oset), t)
{
}

static void thread_eval(AtomSpace* as,
                        const Handle& evelnk, AtomSpace* scratch,
                        bool silent)
{
	prctl(PR_SET_NAME, "atoms:parallel", 0, 0, 0);
	try
	{
		EvaluationLink::do_eval_scratch(as, evelnk, scratch, silent);
	}
	catch (const std::exception& ex)
	{
		logger().warn("Caught exception in thread:\n%s", ex.what());
	}
}

void ParallelLink::evaluate(AtomSpace* as,
                            bool silent,
                            AtomSpace* scratch)
{
	// Create and detach threads; return immediately.
	for (const Handle& h : _outgoing)
	{
		std::thread thr(&thread_eval, as, h, scratch, silent);
		thr.detach();
	}
}

TruthValuePtr ParallelLink::evaluate(AtomSpace* as,
                                     bool silent)
{
	evaluate(as, silent, as);
	return SimpleTruthValue::TRUE_TV();
}

DEFINE_LINK_FACTORY(ParallelLink, PARALLEL_LINK)
