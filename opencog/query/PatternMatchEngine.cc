/*
 * PatternMatchEngine.cc
 *
 * Copyright (C) 2008,2009,2011,2014,2015 Linas Vepstas
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
#include <opencog/util/Logger.h>
#include <opencog/atomutils/FindUtils.h>
#include <opencog/atoms/bind/PatternUtils.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Link.h>
#include <opencog/atomspace/Node.h>

#include "PatternMatchEngine.h"

/* private type */
#define MatchStatus PatternMatchEngine::MatchStatus

using namespace opencog;

// Uncomment below to enable debug print
// #define DEBUG
#ifdef WIN32
#ifdef DEBUG
	#define dbgprt printf
#else
	// something better?
	#define dbgprt
#endif
#else
#ifdef DEBUG
	#define dbgprt(f, varargs...) printf(f, ##varargs)
#else
	#define dbgprt(f, varargs...)
#endif
#endif

static inline void prtmsg(const char * msg, const Handle& h)
{
#ifdef DEBUG
	if (h == Handle::UNDEFINED) {
		printf ("%s (invalid handle)\n", msg);
		return;
	}
	std::string str = h->toShortString();
	printf ("%s %s\n", msg, str.c_str());
#endif
}

/* ======================================================== */

PatternMatchEngine::PatternMatchEngine(PatternMatchCallback& pmcb,
                                       const Variables& v,
                                       const Pattern& p)
	: _pmc(pmcb),
	_classserver(classserver()),
	_varlist(&v),
	_pat(&p)
{
	// current state
	// in_quote = false;
	// depth = 0;

}

/* ======================================================== */

/*
 * explore_neighborhood - explore the local (connected) neighborhood
 * of the starter clause, looking for a match. The idea here is that
 * it is much easier to traverse a connected graph looking for the
 * appropriate subgraph (pattern) than it is to try to explore the
 * whole atomspace, at random.  The user callback `initiate_search()`
 * should call this method, suggesting a clause to start with, and
 * where in the clause the search should begin.
 *
 * Inputs:
 * starter_clause:  one of the top clauses of pattern query
 * starter_atom:    must be a sub-clause of starter_clause
 * hgnd:            candidate grounding for starter_atom; the routine searches
 *                  searches recursively all branches around this atom
 *                  in order to find all matches of starter_clause
 *
 * Returns true if one (or more) matches are found
 *
 * This routine is meant to be invoked on every candidate atom taken
 * from the atom space. That atom is assumed to anchor some part of
 * a graph that hopefully will match the pattern.
 */
bool PatternMatchEngine::explore_neighborhood(const Handle& starter_clause,
                                              const Handle& starter_atom,
                                              const Handle& hgnd)
{
    return explore_redex(starter_clause, starter_atom, hgnd);
}

// TODO: write comment
bool PatternMatchEngine::explore_redex(const Handle& starter_clause,
                                       const Handle& starter_atom,
                                       const Handle& hgnd)
{
	if (starter_atom == Handle::UNDEFINED)
		return false;

	bool found = false;
	MatchStatus match;
	do {
		dbgprt("Match all clauses for starter term=%s, clause=\n%s",
		       starter_term->toShortString().c_str(),
		       starter_clause->toShortString().c_str());
		match = all_clauses_match(starter_clause, starter_atom, hgnd);
		if (match != NOT_MATCHED)
		{
			found = true;
		}
	} while (match == MATCHED);

	return found;
}
/*
	PatternTermSeq starter_terms;
	try {
		starter_terms =
			_pat->connected_terms_map.at({starter_atom, starter_clause});
	} catch (...) {
		dbgprt("Explore redex starter terms not found for atom=%s, "
		       "clause=\n%s", starter_atom->toShortString().c_str(),
		       starter_clause->toShortString().c_str());
		return false;
	};

	bool found = false;
	for (const PatternTermPtr& starter_term : starter_terms)
	{
		dbgprt("Explore redex for starter term=%s, clause=\n%s",
		       starter_term->toString().c_str(),
		       starter_clause->toShortString().c_str());		

		MatchStatus match;
		do {
			match = all_clauses_match(starter_term, hgnd, starter_clause);
			if (match != NOT_MATCHED)
			{
				found = true;
			}
		} while (match == MATCHED)
	}
	return found;
}
*/

// TODO: write comment
MatchStatus PatternMatchEngine::all_clauses_match(const Handle& starter_clause,
                                                  const Handle& starter_term,
                                                  const Handle& hgnd)
{
	// If clauses_stack is empty we start a new search
	// otherwise we take up where we left off.
	if (clauses_stack.empty())
	{
		next_clause = starter_clause;
		next_joint = starter_term;
		push_next_clause();
	}

	while (!clauses_stack.empty())
	{
		const Handle& current_clause = clauses_stack.top();
		const Handle& current_term = joints_stack.top();

		MatchStatus match = clause_match(current_clause, current_term, hgnd);

		// If current clause is matched then get next untried clause push it
		// on stack and attempt to find a match for next clause.
		// Otherwise pop current clause from the stack and continue to search
		// the previous clause for another match. If there is no clause left
		// on the stack then answer NOT_MATCHED because of exhaustion.
		if (match == NOT_MATCHED)
		{
			pop_clause();
			if (!clauses_stack.empty())
				_pmc.pop();
		}
		else
		{
			_pmc.push();
			get_next_untried_clause();

			if (Handle::UNDEFINED == next_clause)
			{
#ifdef DEBUG
				dbgprt ("==================== FINITO!\n");
				print_solution(var_grounding, clause_grounding);
#endif
				if (_pmc.grounding(var_grounding, clause_grounding))
				{
					// TODO: we should reset all internal structures
					return MATCHED_LAST;
				}
				return MATCHED;
			}
			else
			{
				prtmsg("Next clause is\n", next_clause);
				dbgprt("This clause is %s\n",
					is_optional(curr_root)? "optional" : "required");
				dbgprt("This clause is %s\n",
					is_evaluatable(curr_root)?
					"dynamically evaluatable" : "non-dynamic");
				prtmsg("Joining variable  is", next_joint);
				prtmsg("Joining grounding is", var_grounding[next_joint]);
				push_next_clause();
			}
		}
	}

	return NOT_MATCHED;
}

MatchStatus PatternMatchEngine::clause_match(const Handle& clause_root,
                                             const Handle& starter_term,
                                             const Handle& hgnd)
{
	// TODO: optionals support, clause_accept support

	// term_branch_match();
	// bottom_up_branch_match();

	return NOT_MATCHED;

/*
	// TODO: change to stack..
	clause_grounding[clause_root] = hg;
	prtmsg("---------------------\nclause:\n", clause_root);
	prtmsg("ground:", hg)
*/
}

/*
 * This is called when we've navigated to the top of a clause,
 * and so it is fully grounded, and we're essentially done.
 * However, let the callbacks have the final say on whether to
 * proceed onwards, or to backtrack.
 */
bool PatternMatchEngine::clause_accept(const Handle& hp,
                                       const Handle& hg,
                                       const Handle& clause_root)
{
	// Is this clause a required clause? If so, then let the callback
	// make the final decision; if callback rejects, then it's the
	// same as a mismatch; try the next one.
	bool match;
	if (is_optional(clause_root))
	{
		match = _pmc.optional_clause_match(hp, hg);
		dbgprt("optional clause match callback match=%d\n", match);
	}
	else
	{
		match = _pmc.clause_match(hp, hg);
		dbgprt("clause match callback match=%d\n", match);
	}
	return match;
}

/* ======================================================== */

/*
 * Search for the next untried, (thus ungrounded, unsolved) clause.
 *
 * The "issued" set contains those clauses which are currently in play,
 * i.e. those for which a grounding is currently being explored. Both
 * grounded, and as-yet-ungrounded clauses may be in this set.  The
 * sole reason of this set is to avoid infinite resursion, i.e. of
 * re-identifying the same clause over and over as unsolved.
 *
 * The words "solved" and "grounded" are used as synonyms throught the
 * code.
 *
 * Additional complications are introduced by the presence of
 * evaluatable terms, black-box terms, and optional clauses. An
 * evaluatable term is any term that needs to be evaluated to determine
 * if it matches: such terms typically do not exist in the atomspace;
 * they are "virtual", and "exist" only when the evaluation returns
 * "true". Thus, these can only be grounded after all other possible
 * clauses are grounded; thus these are saved for last.  It is always
 * possible to save these for last, because earlier stages have
 * guaranteed that all of he non-virtual clauses are connected.
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
void PatternMatchEngine::get_next_untried_clause(void)
{
	// First, try to ground all the mandatory clauses, only.
	// no virtuals, no black boxes, no optionals.
	if (get_next_thinnest_clause(false, false, false)) return;

	// Don't bother looking for evaluatables if they are not there.
	if (not _pat->evaluatable_holders.empty())
	{
		if (get_next_thinnest_clause(true, false, false)) return;
		if (not _pat->black.empty())
		{
			if (get_next_thinnest_clause(true, true, false)) return;
		}
	}

	// If there are no optional clauses, we are done.
	if (_pat->optionals.empty())
	{
		// There are no more ungrounded clauses to consider. We are done.
		next_clause = Handle::UNDEFINED;
		next_joint = Handle::UNDEFINED;
		return;
	}

	// Try again, this time, considering the optional clauses.
	if (get_next_thinnest_clause(false, false, true)) return;
	if (not _pat->evaluatable_holders.empty())
	{
		if (get_next_thinnest_clause(true, false, true)) return;
		if (not _pat->black.empty())
		{
			if (get_next_thinnest_clause(true, true, true)) return;
		}
	}

	// If we are here, there are no more unsolved clauses to consider.
	next_clause = Handle::UNDEFINED;
	next_joint = Handle::UNDEFINED;
}

/*
 * Count the number of ungrounded variables in a clause.
 *
 * This is used to search for the "thinnest" ungrounded clause:
 * the one with the fewest ungrounded variables in it. Thus, if
 * there is just one variable that needs to be grounded, then this
 * can be done in a direct fashion; it resembles the concept of
 * "unit propagation" in the DPLL algorithm.
 *
 * XXX TODO ... Rather than counting the number of variables, we
 * should instead look for one with the smallest incoming set.
 * That is because the very next thing that we do will be to
 * iterate over the incoming set of "pursue" ... so it could be
 * a huge pay-off to minimize this.
 *
 * If there are two ungrounded variables in a clause, then the
 * "thickness" is the *product* of the sizes of the two incoming
 * sets. Thus, the fewer ungrounded variables, the better.
 *
 * Danger: this assumes a suitable dataset, as otherwise, the cost
 * of this "optimization" can add un-necessarily to the overhead.
 */
unsigned int PatternMatchEngine::thickness(const Handle& clause,
                                           const std::set<Handle>& live)
{
	// If there are only zero or one ungrounded vars, then any clause
	// will do. Blow this pop stand.
	if (live.size() < 2) return 1;

	unsigned int count = 0;
	for (const Handle& v : live)
	{
		if (is_unquoted_in_tree(clause, v)) count++;
	}
	return count;
}

/*
 * Same as above, but with three boolean flags:  if not set, then only
 * those clauses satsifying the criterion are considered, else all
 * clauses are considered.
 *
 * Return true if we found the next ungrounded clause.
 */
bool PatternMatchEngine::get_next_thinnest_clause(bool search_virtual,
                                                  bool search_black,
                                                  bool search_optionals)
{
	// Search for an as-yet ungrounded clause. Search for required
	// clauses first; then, only if none of those are left, move on
	// to the optional clauses.  We can find ungrounded clauses by
	// looking at the grounded vars, looking up the root, to see if
	// the root is grounded.  If its not, start working on that.
	Handle joint(Handle::UNDEFINED);
	Handle unsolved_clause(Handle::UNDEFINED);
	unsigned int thinnest_joint = UINT_MAX;
	unsigned int thinnest_clause = UINT_MAX;
	bool unsolved = false;

	// Make a list of the as-yet ungrounded variables.
	std::set<Handle> ungrounded_vars;

	// Grounded variables ordered by the size of their grounding incoming set
	std::multimap<std::size_t, Handle> thick_vars;

	for (const Handle &v : _varlist->varset)
	{
		try {
			const Handle& gnd = var_grounding.at(v);
			if (Handle::UNDEFINED != gnd)
			{
				std::size_t incoming_set_size = gnd->getIncomingSetSize();
				thick_vars.insert(std::make_pair(incoming_set_size, v));
			}
			else ungrounded_vars.insert(v);
		}
		catch(...) { ungrounded_vars.insert(v); }
	}

	// We are looking for a joining atom, one that is shared in common
	// with the a fully grounded clause, and an as-yet ungrounded clause.
	// The joint is called "pursue", and the unsolved clause that it
	// joins will become our next untried clause. We choose joining atom
	// with smallest size of its incoming set. If there are many such
	// atoms we choose one from clauses with minimal number of ungrounded
	// yet variables.
	for (auto tckvar : thick_vars)
	{
		std::size_t pursue_thickness = tckvar.first;
		const Handle& pursue = tckvar.second;

		if (pursue_thickness > thinnest_joint) break;

		try { _pat->connectivity_map.at(pursue); }
		catch(...) { continue; }
		const Pattern::RootList& rl(_pat->connectivity_map.at(pursue));

		for (const Handle& root : rl)
		{
			if ((issued_clauses.end() == issued_clauses.find(root))
			        and (search_virtual or not is_evaluatable(root))
			        and (search_black or not is_black(root))
			        and (search_optionals or not is_optional(root)))
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

	if (unsolved)
	{
		// Joint is a (variable) node that's shared between several
		// clauses. One of the clauses has been grounded, another
		// has not.  We want to now traverse upwards from this node,
		// to find the top of the ungrounded clause.
		next_clause = unsolved_clause;
		next_joint = joint;
		if (Handle::UNDEFINED != unsolved_clause)
		{
			return true;
		}
	}

	return false;
}

void PatternMatchEngine::push_next_clause(void)
{
	issued_clauses.insert(next_clause);
	clauses_stack.push(next_clause);
	joints_stack.push(next_joint);

	// TODO: clause_grounding ...
	// TODO: variable_grounding ...
}

void PatternMatchEngine::pop_clause(void)
{
	// TODO: clause_grounding ...
	// TODO: variable_grounding ...

	issued_clauses.erase(clauses_stack.top());
	clauses_stack.pop();
	joints_stack.pop();
}

/* ======================================================== */

void PatternMatchEngine::print_solution(
	const std::map<Handle, Handle> &vars,
	const std::map<Handle, Handle> &clauses)
{
	printf("\nVariable groundings:\n");

	// Print out the bindings of solutions to variables.
	std::map<Handle, Handle>::const_iterator j = vars.begin();
	std::map<Handle, Handle>::const_iterator jend = vars.end();
	for (; j != jend; ++j)
	{
		Handle var(j->first);
		Handle soln(j->second);

		// Only print grounding for variables.
		if (VARIABLE_NODE != var->getType()) continue;

		if (soln == Handle::UNDEFINED)
		{
			printf("ERROR: ungrounded variable %s\n",
			       var->toShortString().c_str());
			continue;
		}

		printf("\t%s maps to %s\n",
		       var->toShortString().c_str(),
		       soln->toShortString().c_str());
	}

	// Print out the full binding to all of the clauses.
	printf("\nGrounded clauses:\n");
	std::map<Handle, Handle>::const_iterator m;
	int i = 0;
	for (m = clauses.begin(); m != clauses.end(); ++m, ++i)
	{
		if (m->second == Handle::UNDEFINED)
		{
			Handle mf(m->first);
			prtmsg("ERROR: ungrounded clause: ", mf);
			continue;
		}
		std::string str = m->second->toShortString();
		printf ("%d.   %s\n", i, str.c_str());
	}
	printf ("\n");
}

/* ===================== END OF FILE ===================== */
