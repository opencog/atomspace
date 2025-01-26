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

#include <opencog/util/oc_assert.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/UnorderedLink.h>
#include <opencog/atoms/pattern/PatternLink.h>

#include "Satisfier.h"

using namespace opencog;

bool Satisfier::propose_grounding(const GroundingMap &var_soln,
                                  const GroundingMap &term_soln)
{
	LOCK_PE_MUTEX;
	// PatternMatchEngine::print_solution(var_soln, term_soln);
	_result = true;

	// XXX Temp hack alert. When Continuations finally terminate, they
	// supply us with empty groundings. This probably needs to be fixed
	// someday. For now, for the simple examples, its good enough.
	if (0 == var_soln.size()) return true;

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
			// Optional clauses (e.g. AbsentLink) may have
			// variables in them that are not grounded.
			// Those variables won't have a grounding;
			// this will cause std::map::at to throw.
			try
			{
				vargnds.push_back(var_soln.at(hv));
			}
			catch (...)
			{
				vargnds.push_back(hv);
			}
		}
		_ground = createLink(std::move(vargnds), LIST_LINK);
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
	if (_result) return done;

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

	GroundingMap empty;
	bool rc = eval_sentence(_pattern_body, empty);
	if (rc)
		_result = true;

	return rc;
}

// ===========================================================

bool SatisfyingSet::satisfy(const PatternLinkPtr& plp)
{
	return ContinuationMixin::satisfy(plp);
}

ValuePtr SatisfyingSet::wrap_result(const GroundingMap &var_soln)
{
	_num_results ++;

	if (1 == _varseq.size())
	{
		// std::map::at() can throw. Rethrow for easier deubugging.
		try
		{
			return var_soln.at(_varseq[0]);
		}
		catch (...)
		{
			OC_ASSERT(false,
				"Internal error: ungrounded variable %s\n",
				_varseq[0]->to_string().c_str());
		}
	}

	// If more than one variable, encapsulate in sequential order,
	// in a ListLink.
	std::vector<ValuePtr> vargnds;
	for (const Handle& hv : _varseq)
	{
		// Optional clauses (e.g. AbsentLink) may have variables
		// in them that are not grounded. Those variables won't
		// have a grounding; this will cause std::map::at to throw.
		try
		{
			vargnds.push_back(var_soln.at(hv));
		}
		catch (...)
		{
			vargnds.push_back(hv);
		}
	}
	return createLinkValue(std::move(vargnds));
}

// MeetLink and GetLink groundings go through here.
bool SatisfyingSet::propose_grounding(const GroundingMap &var_soln,
                                      const GroundingMap &term_soln)
{
	LOCK_PE_MUTEX;
	// PatternMatchEngine::log_solution(var_soln, term_soln);

	// Do not accept new solution if maximum number has been already reached
	if (_num_results >= max_results)
		return true;

	_result_queue->add(std::move(wrap_result(var_soln)));

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
bool SatisfyingSet::propose_grouping(const GroundingMap &var_soln,
                                     const GroundingMap &term_soln,
                                     const GroundingMap &grouping)
{
	// Do not accept new solution if maximum number has been already reached
	if (_num_results >= max_results)
		return true;

	// Place the result into the indicated grouping.
	ValueSet& grp = _groups[grouping];
	grp.insert(wrap_result(var_soln));

	return false;
}

bool SatisfyingSet::start_search(void)
{
	if (_result_queue->is_closed())
	{
		_result_queue->clear();
		_result_queue->open();
	}
	return false;
}

bool SatisfyingSet::search_finished(bool done)
{
	// If there are groupings, report them now.
	// Report only those groupings in the requested size range.
	size_t gmin = _pattern->group_min_size;
	size_t gmax = ULONG_MAX;
	if (0 < _pattern->group_max_size) gmax = _pattern->group_max_size;
	for (const auto& gset : _groups)
	{
		size_t gsz = gset.second.size();
		if (gmin <= gsz and gsz <= gmax)
			_result_queue->add(std::move(createLinkValue(gset.second)));
	}

	_result_queue->close();
	return done;
}

/* ===================== END OF FILE ===================== */
