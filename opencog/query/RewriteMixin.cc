/*
 * RewriteMixin.cc
 *
 * Copyright (C) 2009, 2014 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/pattern/BindLink.h>

#include "RewriteMixin.h"

using namespace opencog;

RewriteMixin::RewriteMixin(AtomSpace* as)
	: _as(as), inst(as), max_results(SIZE_MAX)
{
}

/**
 * This callback takes the reported grounding, runs it through the
 * instantiator, to create the implicand, and then records the result
 * in the `result_set`. Repeated solutions are skipped. If the number
 * of unique results so far is less than `max_results`, it then returns
 * false, to search for more groundings.  (The engine will halt its
 * search for a grounding once an acceptable one has been found; so,
 * to continue hunting for more, we return `false` here. We want to
 * find all possible groundings.)
 */
bool RewriteMixin::propose_grounding(const GroundingMap &var_soln,
                                     const GroundingMap &term_soln)
{
	LOCK_PE_MUTEX;
	// PatternMatchEngine::print_solution(var_soln, term_soln);

	// Catch and ignore SilentExceptions. This arises when
	// running with the URE, which creates ill-formed links
	// (due to rules producing nothing). Ideally this should
	// be treated as a user error, that is, the user should
	// design rule pre-conditions to prevent them from producing
	// nothing.  In practice it is difficult to insure, so
	// meanwhile this try-catch is used.
	// See issue #950 and pull req #962. XXX FIXME later.
	// Tested by BuggyBindLinkUTest and NoExceptionUTest.
	try {
		for (const Handle& himp: implicand)
		{
			ValuePtr v(inst.instantiate(himp, var_soln, true));
			insert_result(v);
		}
	} catch (const SilentException& ex) {}

	// If we found as many as we want, then stop looking for more.
	return (_result_set.size() >= max_results);
}

void RewriteMixin::insert_result(ValuePtr v)
{
	if (nullptr == v) return;
	if (_result_set.end() != _result_set.find(v)) return;

	// Insert atom into the atomspace immediately, so that it
	// becomes visible in other threads. XXX Is this really needed?
	if (v->is_atom())
		v = _as->add_atom(HandleCast(v));

	if (_result_set.end() != _result_set.find(v)) return;

	_result_set.insert(v);
	_result_queue->push(std::move(v));
}

bool RewriteMixin::start_search(void)
{
	// *Every* search gets a brand new, fresh queue!
	// This allows users to hang on to the old queue, holding
	// previous results, if they need to.
	_result_queue = createQueueValue();
	return false;
}

bool RewriteMixin::search_finished(bool done)
{
	_result_queue->close();
	return done;
}

/* ===================== END OF FILE ===================== */
