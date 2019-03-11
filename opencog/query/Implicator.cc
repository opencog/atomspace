/*
 * Implicator.cc
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

#include "DefaultImplicator.h"

using namespace opencog;

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
bool Implicator::grounding(const HandleMap &var_soln,
                           const HandleMap &term_soln)
{
	// PatternMatchEngine::print_solution(term_soln,var_soln);

	// Do not accept new solution if maximum number has been already reached
	if (_result_set.size() >= max_results)
		return true;

	// Ignore the case where the URE creates ill-formed links (due to
	// rules producing nothing). Ideally this should be treated as a
	// user error, that is, the user should design rule pre-conditions
	// to prevent them from producing nothing.  In practice it is
	// difficult to insure, so meanwhile this try-catch is used.
	// See issue #950 and pull req #962. XXX FIXME later.
	try {
		ValuePtr v(inst.instantiate(implicand, var_soln, true));
		insert_result(v);
	} catch (const SilentException& ex) {}

	// If we found as many as we want, then stop looking for more.
	return (_result_set.size() >= max_results);
}

void Implicator::insert_result(const ValuePtr& v)
{
	if (v and _result_set.end() == _result_set.find(v))
	{
		// Insert atom into the atomspace immediately, so that
		// it becomes visible in other threads.
		if (v->is_atom())
		{
			_result_set.insert(_as->add_atom(HandleCast(v)));
		}
		else
		{
			_result_set.insert(v);
		}
	}
}

/* ===================== END OF FILE ===================== */
