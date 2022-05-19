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
#include <opencog/atoms/core/FindUtils.h>
#include "PatternUtils.h"

using namespace opencog;

namespace opencog {

bool is_black_box(const Handle& clause)
{
	return
		contains_atomtype(clause, GROUNDED_PREDICATE_NODE);
}

bool can_evaluate(const Handle& clause)
{
	Type ct = clause->get_type();
	bool evaluatable =
		// If it is an EvaluatableLink, then is is evaluatable,
		// unless it is one of the pattern-matching links, or
		// if it is a non-gpn EvaluationLink (i.e. a plain
		// old PredicateNode style EvaluationLink).
		// unless it is a closed (variable-free) EvaluationLink
		// over a PredicateNode.
		(nameserver().isA(ct, EVALUATABLE_LINK)
		   and not (PRESENT_LINK == ct)
		   and not (ABSENT_LINK == ct)
		   and not (CHOICE_LINK == ct)
		   and (not (EVALUATION_LINK == ct)
		      or 0 == clause->get_arity()
		      or nameserver().isA(clause->getOutgoingAtom(0)->get_type(),
		                          EVALUATABLE_LINK)))

		// XXX FIXME Are the below needed?
		or contains_atomtype(clause, DEFINED_PREDICATE_NODE)
		or contains_atomtype(clause, DEFINED_SCHEMA_NODE)
		or is_black_box(clause);

	return evaluatable;
}

bool is_constant(const HandleSet& vars, const Handle& clause)
{
	return
		not any_unquoted_unscoped_in_tree(clause, vars)
		and not can_evaluate(clause);
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
 * by exploring the combinatoric possibilities presented by the virtual
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
 */

// A clause is connected if any of the `cur_vars` appear in the clause.
// If the clause has NO variables, then its connected to anything.
bool is_connected(const Handle& cl, const HandleSet& cur_vars)
{
	// The likely case.
	if (any_unquoted_in_tree(cl, cur_vars)) return true;

	// Unusual case: clause has no variables in it!
	// This will connect to anything.
	if (not contains_atomtype(cl, VARIABLE_NODE) and
	    not contains_atomtype(cl, GLOB_NODE)) return true;

	return false;
}

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

			// Which component might this clause possibly belong to?
			// Try them all.
			size_t nc = components.size();
			for (size_t i = 0; i<nc; i++)
			{
				HandleSet& cur_vars(component_vars[i]);
				// If clause cl is connected to this component,
				// then add it to this component.
				if (is_connected(cl, cur_vars))
				{
					// Extend this component
					components[i].emplace_back(cl);

					// Add to the varset cache for this component.
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

void get_bridged_components(const HandleSet& vars,
                            const PatternTermSeq& prsnts,
                            const PatternTermSeq& absnts,
                            HandleSeqSeq& components,
                            HandleSetSeq& component_vars)
{
	HandleSeq nonopts;
	for (const PatternTermPtr& ptm: prsnts)
		nonopts.emplace_back(ptm->getQuote());

	HandleSeq opts;
	for (const PatternTermPtr& ptm: absnts)
		opts.emplace_back(ptm->getQuote());

	if (0 == opts.size())
	{
		get_connected_components(vars, nonopts, components, component_vars);
		return;
	}

	if (0 == nonopts.size())
	{
		get_connected_components(vars, opts, components, component_vars);
		return;
	}

	// Some optionals bridge across components; others simply
	// connect to some of them. We need to figure out which is which.
	get_connected_components(vars, nonopts, components, component_vars);

	// Now try to attach opts.
	for (const Handle& opt: opts)
	{
		// Count how many components this opt might attach to.
		size_t cnt = 0;
		for (const HandleSet& vars: component_vars)
		{
			if (any_unquoted_in_tree(opt, vars)) cnt++;
		}

		// If its not attached to anything, create a new component.
		if (0 == cnt)
		{
			components.push_back({opt});

			FindAtoms fv(vars);
			fv.search_set(opt);
			component_vars.emplace_back(fv.varset);
		}
		else if (1 == cnt)
		{
			// Attach it to that one component.
			get_connected_components(vars, {opt}, components, component_vars);
		}

		// else `(1 < cnt)` and its a bridge. Do nothing.
	}

	// The above loop has a very subtle bug, which I am going to
	// ignore, because its hard to fix, and unlikely to get triggered.
	// Yet, it could happen: so here goes. If one opt clause shares
	// variables with another opt clause, yet each of these two
	// share variables with two distinctly different components,
	// then these two opts, together, bridge between the components.
	// The user might want to reject such bridges, but allow each
	// opt individually, as long as they don't bridge. As of today,
	// specifying this kind of pattern would take some hard work,
	// (I'm not sure its even possible with today's API) and so it
	// seems very unlikely that any user would want this, and thus
	// very unlikely that they'll hit this bug.
}

} // namespace opencog

/* ===================== END OF FILE ===================== */
