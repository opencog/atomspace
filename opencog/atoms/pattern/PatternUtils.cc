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

/**
 * Remove constant clauses from the list of clauses. Every clause
 * should contain at least one variable, or it should be evaluatable.
 * If does not, or is not, remove the clause from the list of clauses.
 *
 * The core idea is that pattern matching against a constant expression
 * "doesn't make sense" -- the constant expression will always match to
 * itself and is thus "trivial".  In principle, the programmer should
 * never include constants in the list of clauses ... but, due to
 * programmer error, this can happen, and will lead to failures during
 * pattern matching. Thus, the routine below can be used to clean up
 * the pattern-matcher input.
 *
 * Terms that contain GroundedSchema or GroundedPredicate nodes can
 * have side-effects, and are thus are not constants, even if they
 * don't contain any variables. They must be kept around, and must be
 * evaluated during the pattern search.  The definitions of
 * DefinedPredicate or DefinedSchema nodes cannot be accessed until
 * runtime evaluation/execution, so these too must be kept.
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
bool remove_constants(const HandleSet& vars, Pattern& pat)
{
	bool modified = false;

	// Caution: this loop modifies the clauses list!
	HandleSeq::iterator i;
	for (i = pat.mandatory.begin(); i != pat.mandatory.end();)
	{
		Handle clause(*i);

		if (not is_constant(vars, clause))
		{
			++i; continue;
		}

		i = pat.mandatory.erase(i);

		// Remove the clause from quoted_clauses.
		auto qc = std::find(pat.quoted_clauses.begin(),
		                   pat.quoted_clauses.end(), clause);
		if (qc != pat.quoted_clauses.end())
			pat.quoted_clauses.erase(qc);

		// Remove the clause from unquoted_clauses.
		auto uc = std::find(pat.unquoted_clauses.begin(),
		                   pat.unquoted_clauses.end(), clause);
		if (uc != pat.unquoted_clauses.end())
			pat.unquoted_clauses.erase(uc);

		modified = true;
	}

	return modified;
}

bool is_constant(const HandleSet& vars, const Handle& clause)
{
	Type ct = clause->get_type();
	bool constant =
		not (any_unquoted_unscoped_in_tree(clause, vars)
		     or contains_atomtype(clause, DEFINED_PREDICATE_NODE)
		     or contains_atomtype(clause, DEFINED_SCHEMA_NODE)
		     or contains_atomtype(clause, GROUNDED_PREDICATE_NODE)
		     or contains_atomtype(clause, GROUNDED_SCHEMA_NODE)
		     or contains_atomtype(clause, PREDICATE_FORMULA_LINK)
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

// Unfortunately for us, the list of clauses that we were given
// includes the optionals. It might be nice if this was fixed
// upstream, but that seems to be hard. XXX FIXME. So, here,
// we brute-force remove them.
static HandleSeq get_nonopts(const HandleSeq& clauses,
                             const HandleSeq& opts)
{
	HandleSeq nonopts;
	for (const Handle& h: clauses)
	{
		bool is_opt = false;
		for (const Handle& opt: opts)
		{
			if (h == opt) { is_opt = true; break; }
		}
		if (not is_opt) nonopts.emplace_back(h);
	}
	return nonopts;
}

void get_bridged_components(const HandleSet& vars,
                            const HandleSeq& clauses,
                            const HandleSeq& opts,
                            HandleSeqSeq& components,
                            HandleSetSeq& component_vars)
{
	if (0 == opts.size())
	{
		get_connected_components(vars, clauses, components, component_vars);
		return;
	}

	// Some optionals bridge across components; others simply
	// connect to some of them. We need to figure out which is which.

	HandleSeq nonopts(get_nonopts(clauses, opts));
	if (0 == nonopts.size())
	{
		get_connected_components(vars, opts, components, component_vars);
		return;
	}

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
	// (I'm not sure its even possible with toeay's API) and so it
	// seems very unlikely that any user would want this, and thus
	// very unlikely that they'll hit this bug.
}

} // namespace opencog

/* ===================== END OF FILE ===================== */
