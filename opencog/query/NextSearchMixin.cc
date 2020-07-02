/*
 * NextSearchMixin.cc
 *
 * Copyright (C) 2008,2009,2011,2014,2015,2020 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  February 2008
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
#include <opencog/atoms/core/FindUtils.h>

#include "InitiateSearchMixin.h"
#include "PatternMatchEngine.h"

// #define QDEBUG 1
#ifdef QDEBUG
#define DO_LOG(STUFF) STUFF
#else
#define DO_LOG(STUFF)
#endif

using namespace opencog;

/* ======================================================== */

void InitiateSearchMixin::push(void)
{
	_issued_stack.push(_issued);
}

void InitiateSearchMixin::pop(void)
{
	_issued = _issued_stack.top();
	_issued_stack.pop();
}

/**
 * Search for the next untried, (thus ungrounded, unsolved) clause.
 *
 * The "issued" set contains those clauses which are currently in play,
 * i.e. those for which a grounding is currently being explored. Both
 * grounded, and as-yet-ungrounded clauses may be in this set.  The
 * sole reason of this set is to avoid infinite recursion, i.e. of
 * re-identifying the same clause over and over as unsolved.
 *
 * The words "solved" and "grounded" are used as synonyms through out
 * the code.
 *
 * Additional complications are introduced by the presence of
 * evaluatable terms, black-box terms, and optional clauses. An
 * evaluatable term is any term that needs to be evaluated to determine
 * if it matches: such terms typically do not exist in the atomspace;
 * they are "virtual", and "exist" only when the evaluation returns
 * "true". Thus, these can only be grounded after all other possible
 * clauses are grounded; thus these are saved for last.  It is always
 * possible to save these for last, because earlier stages have
 * guaranteed that all of the non-virtual clauses are connected.
 * Anyway, evaluatables come in two forms: those that can be evaluated
 * quickly, and those that require a "black-box" evaluation of some
 * scheme or python code. Of the two, we save "black-box" for last.
 *
 * Then, after grounding all of the mandatory clauses (virtual or not),
 * we look for optional clauses, if any. Again, these might be virtual,
 * and they might be black...
 *
 * Thus, we use a helper function to broaden the search in each case.
 */
bool InitiateSearchMixin::get_next_clause(const GroundingMap& var_grounding,
                                          PatternTermPtr& clause,
                                          Handle& joint)
{
	if (0 < _next_choices.size())
	{
		const Choice& ch(_next_choices.back());
		clause = ch.clause;
		joint = ch.start_term;
		_next_choices.pop_back();
		return true;
	}

	get_next_untried(var_grounding);
	if (0 == _next_choices.size()) return false;

	const Choice& ch(_next_choices.back());
	clause = ch.clause;
	joint = ch.start_term;
	_next_choices.pop_back();
	return true;
}

void InitiateSearchMixin::get_next_untried(const GroundingMap& var_grounding)
{
	// First, try to ground all the mandatory clauses, only.
	// no virtuals, no black boxes, no absents.
	if (get_next_thinnest_clause(var_grounding, false, false)) return;

	// Don't bother looking for evaluatables if they are not there.
	if (_pattern->have_evaluatables)
	{
		if (get_next_thinnest_clause(var_grounding, true, false)) return;
	}

	// Try again, this time, considering the absent clauses.
	if (not _pattern->absents.empty())
	{
		if (get_next_thinnest_clause(var_grounding, false, true)) return;
		if (_pattern->have_evaluatables)
		{
			if (get_next_thinnest_clause(var_grounding, true, true)) return;
		}
	}

	// Now loop over all for-all clauses.
	// All variables must neccessarily be grounded at this point.
	for (const PatternTermPtr& root : _pattern->always)
	{
		if (_issued.end() != _issued.find(root)) continue;
		_issued.insert(root);
		for (const Handle &v : _variables->varset)
		{
			if (is_free_in_tree(root->getHandle(), v))
			{
				Choice ch;
				ch.clause = root;
				ch.start_term = v;
				_next_choices.emplace_back(ch);
				return;
			}
		}
		throw RuntimeException(TRACE_INFO, "BUG! Somethings wrong!!");
	}

	// Make sure all clauses have been grounded.
	for (const PatternTermPtr& root : _pattern->pmandatory)
	{
		if (_issued.end() == _issued.find(root))
			throw RuntimeException(TRACE_INFO,
				"BUG! Still have ungrounded clauses!!");
	}
}

// Count the number of ungrounded variables in a clause.
//
// This is used to search for the "thinnest" ungrounded clause:
// the one with the fewest ungrounded variables in it. Thus, if
// there is just one variable that needs to be grounded, then this
// can be done in a direct fashion; it resembles the concept of
// "unit propagation" in the DPLL algorithm.
//
// XXX TODO ... Rather than counting the number of variables, we
// should instead look for one with the smallest incoming set.
// That is because the very next thing that we do will be to
// iterate over the incoming set of "pursue" ... so it could be
// a huge pay-off to minimize this.
//
// If there are two ungrounded variables in a clause, then the
// "thickness" is the *product* of the sizes of the two incoming
// sets. Thus, the fewer ungrounded variables, the better.
//
// Danger: this assumes a suitable dataset, as otherwise, the cost
// of this "optimization" can add un-necessarily to the overhead.
//
unsigned int InitiateSearchMixin::thickness(const PatternTermPtr& clause,
                                            const HandleSet& live)
{
	// If there are only zero or one ungrounded vars, then any clause
	// will do. Blow this pop stand.
	if (live.size() < 2) return 1;

	const Handle& hclause = clause->getHandle();
	unsigned int count = 0;
	for (const Handle& v : live)
	{
		if (is_unquoted_in_tree(hclause, v)) count++;
	}
	return count;
}

/// get_glob_embedding() -- given glob node, return term that it grounds.
///
/// If a GlobNode has a grounding, then there is always some
/// corresponding term which contains that grounded GlobNode and is
/// grounded. If that term appears in two (or more) clauses, then
/// return it, so that it is used as the pivot point to the next
/// ungrounded clause.  If there is no such term, then just  return the
/// glob.
Handle InitiateSearchMixin::get_glob_embedding(const GroundingMap& var_grounding,
                                               const Handle& glob)
{
	// If the glob is in only one clause, there is no connectivity map.
	if (0 == _pattern->connectivity_map.count(glob)) return glob;

	// Find some clause, any clause at all, containg the glob,
	// that has not been grounded so far. We need to do this because
	// the glob might appear in three clauses, with two of them
	// grounded by a common term, and the third ungrounded
	// with no common term.
	const auto& clauses =  _pattern->connectivity_map.equal_range(glob);
	auto clpr = clauses.first;
	for (; clpr != clauses.second; clpr++)
	{
		if (_issued.end() == _issued.find(clpr->second)) break;
	}

	// Glob is not in any ungrounded clauses.
	if (clpr == clauses.second) return glob;

	std::pair<Handle, PatternTermPtr> glbt({glob, clpr->second});
	const auto& ptms = _pattern->connected_terms_map.find(glbt);
	const PatternTermPtr& ptm = ptms->second[0];

	// Here, ptm is the glob itself. It will almost surely
	// be in some term. The test for nullptr will surely never
	// trigger.
	const PatternTermPtr& parent = ptm->getParent();
	if (nullptr == parent) return glob;

	// If this term appears in more than one clause, then it
	// can be used as a pivot.
	const Handle& embed = parent->getHandle();
	if ((var_grounding.end() != var_grounding.find(embed)) and
	    (1 < _pattern->connectivity_map.count(embed)))
		return embed;

	return glob;
}

/// Work-horse for `get_next_untried_clause`. Locates the actual clause
/// to run next. Takes three boolean flags: When the boolean is not set,
/// then only those clauses satsifying the criterion are considered,
/// else all clauses are considered.
///
/// Return true if we found the next ungrounded clause.
bool InitiateSearchMixin::get_next_thinnest_clause(const GroundingMap& var_grounding,
                                                   bool search_eval,
                                                   bool search_absents)
{
	// Search for an as-yet ungrounded clause. Search for required
	// clauses first; then, only if none of those are left, move on
	// to the optional clauses.  We can find ungrounded clauses by
	// looking at the grounded vars, looking up the root, to see if
	// the root is grounded.  If its not, start working on that.
	Handle joint(Handle::UNDEFINED);
	PatternTermPtr unsolved_clause(PatternTerm::UNDEFINED);
	unsigned int thinnest_joint = UINT_MAX;
	unsigned int thinnest_clause = UINT_MAX;
	bool unsolved = false;

	// Make a list of the as-yet ungrounded variables.
	HandleSet ungrounded_vars;

	// Grounded variables ordered by the size of their grounding incoming set
	std::multimap<std::size_t, Handle> thick_vars;

	for (const Handle &v : _variables->varset)
	{
		const auto& gnd = var_grounding.find(v);
		if (gnd != var_grounding.end())
		{
			// We cannot use GlobNode's directly as joiners, because
			// we don't know how they fit. Instead, we have to fish
			// out a term that contains a grounded glob, and use
			// that term as the joiner.
			if (GLOB_NODE == v->get_type())
			{
				Handle embed = get_glob_embedding(var_grounding, v);
				const Handle& tg = var_grounding.find(embed)->second;
				std::size_t incoming_set_size = tg->getIncomingSetSize();
				thick_vars.insert(std::make_pair(incoming_set_size, embed));
			}
			else
			{
				std::size_t incoming_set_size = gnd->second->getIncomingSetSize();
				thick_vars.insert(std::make_pair(incoming_set_size, v));
			}
		}
		else ungrounded_vars.insert(v);
	}

	// We are looking for a joining atom, one that is shared in common
	// with the a fully grounded clause, and an as-yet ungrounded clause.
	// The joint is called "pursue", and the unsolved clause that it
	// joins will become our next untried clause. We choose joining atom
	// with smallest size of its incoming set. If there are many such
	// atoms we choose one from clauses with minimal number of ungrounded
	// yet variables.
	for (const auto& tckvar : thick_vars)
	{
		std::size_t pursue_thickness = tckvar.first;
		const Handle& pursue = tckvar.second;

		if (pursue_thickness > thinnest_joint) break;

		const auto& root_list = _pattern->connectivity_map.equal_range(pursue);
		for (auto it = root_list.first; it != root_list.second; it++)
		{
			const PatternTermPtr& root = it->second;
			if ((_issued.end() == _issued.find(root))
			     and (search_eval or not root->hasAnyEvaluatable())
			     and (search_absents or not root->isAbsent()))
			{
				unsigned int root_thickness = thickness(root, ungrounded_vars);
				if (root_thickness < thinnest_clause)
				{
					thinnest_clause = root_thickness;
					thinnest_joint = pursue_thickness;
					unsolved_clause = root;
					joint = pursue;
					unsolved = true;
				}
			}
		}
	}

	// Some clauses have no variables in them, and so the above loops
	// will not find them. Try these now. This means that the
	// variable-free clauses run last. If the user wants to run them
	// earlier, they can always use a SequentialAndLink.
	if (not unsolved and search_eval)
	{
		for (const PatternTermPtr& root : _pattern->pmandatory)
		{
			if (_issued.end() != _issued.find(root)) continue;

			// Clauses with no variables are (by definition)
			// evaluatable. So we don't check if they're evaluatable.
			const HandleSeq& varseq = _pattern->clause_variables.at(root);
			if (0 == varseq.size())
			{
				unsolved_clause = root;
				joint = root->getHandle();
				unsolved = true;
				break;
			}
		}
	}

	if (unsolved)
	{
		// Joint is a (variable) node that's shared between several
		// clauses. One of the clauses has been grounded, another
		// has not.  We want to now traverse upwards from this node,
		// to find the top of the ungrounded clause.
		if (unsolved_clause)
		{
			if (unsolved_clause->isChoice())
			{
				for (const PatternTermPtr& alt : unsolved_clause->getOutgoingSet())
				{
					Choice ch;
					ch.clause = alt;
					ch.start_term = joint;
					_next_choices.emplace_back(ch);
				}
			}
			else
			{
				Choice ch;
				ch.clause = unsolved_clause;
				ch.start_term = joint;
				_next_choices.emplace_back(ch);
			}

			_issued.insert(unsolved_clause);
			return true;
		}
	}

	return false;
}

/* ===================== END OF FILE ===================== */
