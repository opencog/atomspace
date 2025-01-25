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

RewriteMixin::RewriteMixin(AtomSpace* as, QueueValuePtr& qvp)
	: _as(as), _result_queue(qvp),
	_num_results(0), inst(as), max_results(SIZE_MAX)
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

	// If we found as many as we want, then stop looking for more.
	if (_num_results >= max_results)
		return true;

	_num_results ++;

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
		if (1 == implicand.size())
		{
			ValuePtr v(inst.instantiate(implicand[0], var_soln, true));
			insert_result(v);
		}
		else
		{
			ValueSeq vs;
			for (const Handle& himp: implicand)
			{
				ValuePtr v(inst.instantiate(himp, var_soln, true));
				vs.emplace_back(v);
			}
			insert_result(createLinkValue(vs));
		}
	} catch (const SilentException& ex) {}

	// If we found as many as we want, then stop looking for more.
	return (_num_results >= max_results);
}

/// Much like the above, but groundings are organized into groupings.
/// The primary technical problem here is that we cannot report any
/// search results, until after the search has completed. This is
/// because the very last item to be reported may belong to the very
/// first group. So we sit here, stupidly, and wait for search results
/// to dribble in. Perhaps the engine search could be modified in some
/// clever way to find groupings in a single batch; but for now, I don't
/// see how this could be done.
bool RewriteMixin::propose_grouping(const GroundingMap &var_soln,
                                    const GroundingMap &term_soln,
                                    const GroundingMap &grouping)
{
	// Do not accept new solution if maximum number has been already reached
	if (_num_results >= max_results)
		return true;

	_num_results ++;

	// Obtain the grouping that we'll stuff values into.
	ValueSet& grp = _groups[grouping];

	// Count the group size. After performing the rewrite below
	// (in the instantiate code) the reults might collapse to just
	// one instance per group, thus mis-characterizing the actual
	// group size. So count explicitly.
	_group_sizes[grouping] ++;

	try {
		for (const Handle& himp: implicand)
		{
			ValuePtr v(inst.instantiate(himp, var_soln, true));

			// Insert atom into the atomspace immediately. This avoids having
			// the atom appear twice, once unassigned to any AS, and the other
			// in the AS.
			if (v->is_atom())
				v = _as->add_atom(HandleCast(v));

			grp.insert(v);
		}
	} catch (const SilentException& ex) {}

	return false;
}

void RewriteMixin::insert_result(ValuePtr v)
{
	if (nullptr == v) return;
	if (_result_set.end() != _result_set.find(v)) return;

	// Insert atom into the atomspace immediately. This avoids having
	// the atom appear twice, once unassigned to any AS, and the other
	// in the AS.
	if (v->is_atom())
		v = _as->add_atom(HandleCast(v));

	if (_result_set.end() != _result_set.find(v)) return;

	_result_set.insert(v);
	_result_queue->push(std::move(v));
}

bool RewriteMixin::start_search(void)
{
	_result_queue->clear();
	if (_result_queue->is_closed())
		_result_queue->open();
	return false;
}

bool RewriteMixin::search_finished(bool done)
{
	// If there are groupings, report them now.
	// Report only those groupings in the requested size range.
	size_t gmin = _pattern->group_min_size;
	size_t gmax = ULONG_MAX;
	if (0 < _pattern->group_max_size) gmax = _pattern->group_max_size;
	for (const auto& gset : _groups)
	{
		// size_t gsz = gset.second.size();
		size_t gsz = _group_sizes[gset.first];
		if (gmin <= gsz and gsz <= gmax)
			_result_queue->push(createLinkValue(gset.second));
	}

	_result_queue->close();
	return done;
}

/* ===================== END OF FILE ===================== */
