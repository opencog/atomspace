/*
 * Satisfier.cc
 *
 * Copyright (C) 2015 Linas Vepstas
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
#include <opencog/atoms/core/UnorderedLink.h>
#include <opencog/atoms/pattern/PatternLink.h>

#include "Satisfier.h"

using namespace opencog;

bool Satisfier::grounding(const HandleMap &var_soln,
                          const HandleMap &term_soln)
{
	// PatternMatchEngine::print_solution(var_soln, term_soln);
	_result = TruthValue::TRUE_TV();

	// Record the grounding; we cache this later.
	if (1 == _varseq.size())
	{
		_ground = var_soln.at(_varseq[0]);
	}
	else
	{
		// If more than one variable, encapsulate in sequential order,
		// in a ListLink.
		HandleSeq vargnds;
		for (const Handle& hv : _varseq)
		{
			vargnds.push_back(var_soln.at(hv));
		}
		_ground = createLink(vargnds, LIST_LINK);
	}

	// No need to look for more groundings as _result isn't going to change
	// and opencog::satisfaction_link only needs the value of _result.
	return true;
}

/// This method handles the case of SequentialAnd, SequentialOr with
/// embedded AbsentLinks, NotLink-PresentLink and some weird
/// combinations of NotLink-ChoiceLink, and so-on.  The idea here is
/// that, if the pattern matcher ran to exhaustion, and NO groundings
/// at all were found, then pattern evaluation may still need to
/// trigger evaluatable clauses that evaluate only when exhaustive
/// search fails.  So, indeed, we do that here.
///
/// Of course, if the pattern had no variables (e.g. a SequenceLink or
/// FallbackLink with only evaluatables), then there cannot be a
/// grounding failure, by definition.  And if there was a grounding,
/// there can be no grounding failure, either. So we only process the
/// case where there are variables, and grounding failed.
bool Satisfier::search_finished(bool done)
{
	if (done) return done;

	// If there were no variables to be grounded, we have nothing to do.
	if (not _have_variables) return done;

	// If there was a grounding, then don't re-run; we're here
	// only to handle the no-groundings case.
	if (TruthValue::TRUE_TV() == _result) return done;

	// _optionals_present will be set to true if some optional clause
	// was grounded. Ergo, its not the no-grounding case.
	if (_optionals_present) return done;

	// Multi-component patterns will not have distinct bodies.
	// A failure to match one of the components is benign, and is
	// treated appropriately upstream. Just return.
	if (nullptr == _pattern_body) return done;

	// Evaluating the pattern body only makes sense if it is sequential
	// (ordered) -- if the body is an unordered AndLink, or if its a
	// ChoiceLink, etc, this makes no sense.
	Type btype = _pattern_body->get_type();
	if (SEQUENTIAL_AND_LINK != btype and SEQUENTIAL_OR_LINK != btype)
		return done;

	HandleMap empty;
	bool rc = eval_sentence(_pattern_body, empty);
	if (rc)
		_result = TruthValue::TRUE_TV();

	return rc;
}

// ===========================================================

bool SatisfyingSet::grounding(const HandleMap &var_soln,
                              const HandleMap &term_soln)
{
	// PatternMatchEngine::log_solution(var_soln, term_soln);

	// Do not accept new solution if maximum number has been already reached
	if (_satisfying_set.size() >= max_results)
		return true;

	if (1 == _varseq.size())
	{
		// std::map::at() can throw. Rethrow for easier deubugging.
		try
		{
			_satisfying_set.emplace(var_soln.at(_varseq[0]));
		}
		catch (...)
		{
			throw AssertionException(TRACE_INFO,
				"Internal error: ungrounded variable %s\n",
				_varseq[0]->to_string().c_str());
		}

		// If we found as many as we want, then stop looking for more.
		return (_satisfying_set.size() >= max_results);
	}

	// If more than one variable, encapsulate in sequential order,
	// in a ListLink.
	HandleSeq vargnds;
	for (const Handle& hv : _varseq)
	{
		vargnds.push_back(var_soln.at(hv));
	}
	_satisfying_set.emplace(createLink(vargnds, LIST_LINK));

	// If we found as many as we want, then stop looking for more.
	return (_satisfying_set.size() >= max_results);
}

/* ===================== END OF FILE ===================== */
