/*
 * PatternUtils.cc
 *
 * Copyright (C) 2008,2009,2011,2014 Linas Vepstas
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

#include <opencog/atomutils/FindUtils.h>
#include <opencog/atomspace/AtomSpace.h>
#include <boost/range/algorithm/find.hpp>
#include "PatternUtils.h"

using namespace opencog;

namespace opencog {

/**
 * Remove constant clauses from the list of clauses if they are in
 * the queried atomspace.
 *
 * Make sure that every clause contains at least one variable;
 * if not, remove the clause from the list of clauses.
 *
 * The core idea is that pattern matching against a constant expression
 * "doesn't make sense" -- the constant expression will always match to
 * itself and is thus "trivial".  In principle, the programmer should
 * never include constants in the list of clauses ... but, due to
 * programmer error, this can happen, and will lead to failures during
 * pattern matching. Thus, the routine below can be used to validate
 * the input.
 *
 * Terms that contain GroundedSchema or GroundedPredicate nodes can
 * have side-effects, and are thus not really constants. They must be
 * evaluated during the pattern search. Terms that contain
 * DefinedPredicate or DefinedSchema nodes are simply not known until
 * runtime evaluation/execution.
 *
 * The match for EvaluatableLink's is meant to solve the problem of
 * evaluating (SatisfactionLink (AndLink (TrueLink))) vs. evaluating
 * (SatisfactionLink (AndLink (FalseLink))), with the first returning
 * TRUE_TV, and the second returning FALSE_TV. Removing these 'constant'
 * terms would alter the result of the evaluation, potentially even
 * leaving an empty (undefined) AndLink. So we cannot really remove
 * them.
 *
 * Returns true if the list of clauses was modified, else returns false.
 */
bool remove_constants(const HandleSet &vars,
                      Pattern &pat,
                      HandleSeqSeq& components,
                      HandleSeq& component_patterns,
                      const AtomSpace &queried_as)
{
	bool modified = false;

	// Caution: this loop modifies the clauses list!
	HandleSeq::iterator i;
	for (i = pat.clauses.begin(); i != pat.clauses.end();)
	{
		Handle clause(*i);

		if (is_constant(vars, clause, &queried_as))
		{
			pat.constants.emplace_back(clause);
			i = pat.clauses.erase(i);

			// remove the clause from components and component_patterns
			auto j = boost::find(components, HandleSeq{clause});
			if (j != components.end())
			{
				components.erase(j);
				if (not component_patterns.empty())
				{
					auto cpj = std::next(component_patterns.begin(),
					                     std::distance(components.begin(), j));
					component_patterns.erase(cpj);
				}
			}

			// remove the clause from _pattern_mandatory.
			auto m = boost::find(pat.mandatory, clause);
			if (m != pat.mandatory.end())
				pat.mandatory.erase(m);

			// remove the clause from _cnf_clauses.
			auto c = boost::find(pat.cnf_clauses, clause);
			if (c != pat.cnf_clauses.end())
				pat.cnf_clauses.erase(c);

			modified = true;
		}
		else
		{
			++i;
		}
	}

	return modified;
}

bool is_in_atomspace(const Handle& handle, const AtomSpace& atomspace)
{
	return (bool)atomspace.get_atom(handle);
}

bool is_constant(const HandleSet& vars, const Handle& clause,
                 const AtomSpace* queried_as)
{
	Type ct = clause->get_type();
	bool constant =
		not (any_unquoted_unscoped_in_tree(clause, vars)
		     or contains_atomtype(clause, DEFINED_PREDICATE_NODE)
		     or contains_atomtype(clause, DEFINED_SCHEMA_NODE)
		     or contains_atomtype(clause, GROUNDED_PREDICATE_NODE)
		     or contains_atomtype(clause, GROUNDED_SCHEMA_NODE)
		     // TODO: should not the below be any VirtualLink?
		     // Or contains any EvaluatableLink ??
		     or contains_atomtype(clause, IDENTICAL_LINK)
		     or contains_atomtype(clause, EQUAL_LINK)
		     or contains_atomtype(clause, GREATER_THAN_LINK)
		     // If it is an EvaluatableLink then is is not a
		     // constant, unless it is a closed EvaluationLink over
		     // a PredicateNode.
		     or (nameserver().isA(ct, EVALUATABLE_LINK)
		         and (0 == clause->get_arity()
		              or
		              clause->getOutgoingAtom(0)->get_type() != PREDICATE_NODE)));

	if (queried_as)
		return constant and is_in_atomspace(clause, *queried_as);
	return constant;
}

/* ======================================================== */
/**
 * Given an input set of clauses, partition this set into its
 * connected components, returning a list of the connected components,
 * and a matching list of the variables that appear in each component.
 * The input set of clauses are assumed to be in conjunctive normal
 * form; i.e. they do NOT contain any OrLinks. If they do, then you
 * must used the `get_disjoined_components()` function, below.
 *
 * Two clauses are "connected" if they both contain a common
 * variable. A connected component is the set of all clauses that are
 * connected to one-another, in some way.
 *
 * This method serves two different purposes.  First, if the
 * pattern does not contain any "virtual" links, then the pattern
 * matcher works correctly only if there is one single, connected
 * component (this is by design, since we don't want to deal with
 * the combinatoric explosion of grounding multiple distinct components).
 * If the pattern does contain "virtual" links, then the connected
 * components should be grounded first, and the results then combined
 * by exploring the combinatoric possibilites presented by the virtual
 * link(s).
 *
 * A side effect of the algorithm is that it sorts the clauses into
 * connected order. That is, given the vector of connected clauses,
 * each element in the vector is connected to some clause that came
 * before it (in the vector).  This is handy, because it guarantees
 * that the next clause must be connected to the previous ones. This
 * speeds up the discovery of the next ungrounded clause: it is
 * trivially just the very next clause in the connected set.  Of
 * course, users will typically never specify clauses in such order.
 *
 * XXX FIXME: It can happen that some clauses have no variables at all
 * in them.  These end up in their own component, which can be
 * extremely confusing.
 */
void get_connected_components(const HandleSet& vars,
                              const HandleSeq& clauses,
                              HandleSeqSeq& components,
                              HandleSetSeq& component_vars)
{
	HandleSeq todo(clauses);

	while (0 < todo.size())
	{
		// no_con_yet == clauses that failed to connect to any existing
		// component.
		HandleSeq no_con_yet;
		bool did_at_least_one = false;

		for (const Handle& cl: todo)
		{
			bool extended = false;

			// Which component might this possibly belong to??? Try them all.
			size_t nc = components.size();
			for (size_t i = 0; i<nc; i++)
			{
				HandleSet& cur_vars(component_vars[i]);
				// If clause cl is connected to this component, then add it
				// to this component.
				if (any_unquoted_in_tree(cl, cur_vars))
				{
					// Extend the component
					components[i].emplace_back(cl);

					// Add to the varset cache for that component.
					FindAtoms fv(vars);
					fv.search_set(cl);
					for (const Handle& v : fv.varset) cur_vars.insert(v);

					extended = true;
					did_at_least_one = true;
					break;
				}
			}

			if (not extended)
				no_con_yet.emplace_back(cl);
		}

		if (did_at_least_one)
		{
			todo = no_con_yet;
			continue;
		}

		// Grab the first clause that failed to attach to something,
		// and use it to start a new component.
		Handle ncl(no_con_yet.back());
		no_con_yet.pop_back();
		todo = no_con_yet;

		// If we are here, we found a disconnected clause.
		// Start a new component
		components.push_back({ncl});

		FindAtoms fv(vars);
		fv.search_set(ncl);
		component_vars.emplace_back(fv.varset);
	}
}

} // namespace opencog

/* ===================== END OF FILE ===================== */
