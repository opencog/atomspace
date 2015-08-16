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
#ifdef DEBUG
#define dbgprt(f, varargs...) printf(f, ##varargs)
#else
#define dbgprt(f, varargs...)
#endif

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

	// graph state
	// _clause_stack_depth = 0;
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
 * grnd:            candidate grounding of starter_atom; the routine searches
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
                                              const Handle& grnd)
{
    return explore_redex(starter_clause, starter_atom, grnd);
}

bool PatternMatchEngine::explore_redex(const Handle& starter_clause,
                                       const Handle& starter_atom,
                                       const Handle& grnd)
{
	// TODO: write comment
	// TODO: starter_clause alse should be PatternTermPtr ...
	//       what if clauses are not unique ...

	if (starter_atom == Handle::UNDEFINED)
		return false;

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

		MatchStatus match = MATCHED;
		while (match == MATCHED)
		{
			match = all_clauses_match(starter_term, grnd, starter_clause);
			if (match != NOT_MATCHED)
			{
				found = true;
			}
		};
	}
	return found;
}

// TODO: write comment
MatchStatus
PatternMatchEngine::all_clauses_match(const PatternTermPtr& starter_term,
                                      const Handle& grnd,
                                      const Handle& starter_clause)
{
	Handle current_clause = starter_clause;
	PatternTermPtr current_term = starter_term;

	return NOT_MATCHED;

/*
    while (true)
    {
        if (clause_match(current_clause, current_term) == MATCHED)
        {
           if (!clause_accept())
       {
           continue;
       }
       current_clause = get_next_untried_clause()
       if (current_clause)
       {
           push(current_clause, current_term)
           // w tym miejscu też zapamietuje się pozycja wartościowania zmiennych
           continue;
       } else {
       }
   } else {
   }

	}
*/
}

MatchStatus
PatternMatchEngine::single_clause_match(const PatternTermPtr& ptm,
                                        const Handle& grnd,
                                        const Handle& clause_root)
{
	return NOT_MATCHED;
}


/* ===================== END OF FILE ===================== */
