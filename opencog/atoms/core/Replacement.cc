/*
 * atoms/core/Replacement.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
 *               2019 SingularityNET Foundation
 *
 * Authors: Linas Vepstas <linasvepstas@gmail.com>  January 2009
 *          Nil Geisweiller <ngeiswei@gmail.com> Oct 2019
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include "ScopeLink.h"

namespace opencog {

/* ================================================================= */

Handle Replacement::replace_nocheck(const Handle& term,
                                    const HandleMap& vm)
{
	HandleSeq to_insert;
	IndexMap insert_index;
	size_t idx = 0;
	for (const auto& pr : vm)
	{
		to_insert.push_back(pr.second);
		insert_index.insert({pr.first, idx});
		idx++;
	}
	return substitute_scoped(term, to_insert, insert_index);
}

/* ================================================================= */

/// Perform beta-reduction on the term.  This is more-or-less a purely
/// syntactic beta-reduction, except for two semantic parts:
///
/// 1. The semantics of QuoteLink, UnquoteLink is honoured, so that
///    quoted variables are not reduced.
///
/// 2. The semantics of scoping (alpha-conversion) is honored, so that
///    any scoped variables with the same name as the free variables
///    are alpha-hidden, possibly alpha-converted if the substituting
///    values are variables of the same name.
Handle Replacement::substitute_scoped(Handle term,
                                      const HandleSeq& args,
                                      const IndexMap& index_map,
                                      bool do_exec,
                                      Quotation quotation)
{
	bool unquoted = quotation.is_unquoted();

	// If we are not in a quote context, and `term` is a variable,
	// then just return the corresponding argument.
	if (unquoted)
	{
		IndexMap::const_iterator idx = index_map.find(term);
		if (idx != index_map.end())
			return args.at(idx->second);
	}

	// If its a node, and its not a variable, then it is a constant,
	// and just return that.
	if (not term->is_link()) return term;

	Type ty = term->get_type();

	// Update for subsequent recursive calls of substitute_scoped
	quotation.update(ty);

	// If the term is a scope the index map might change, to avoid copy
	// we either point to the original map, or the new one.
	const IndexMap* index_map_ptr = &index_map;
	IndexMap hidden_map;

	if (unquoted and nameserver().isA(ty, SCOPE_LINK))
	{
		// Perform alpha-conversion duck-n-cover.

		// If a substituting value is equal to a variable of that scope,
		// then alpha-convert the scope to avoid variable name
		// collision. Loop in the rare case the new names collide.
		while (must_alpha_convert(term, args))
			term = ScopeLinkCast(term)->alpha_convert();

		// Hide any variables of the scope that are to be substituted,
		// that is remove them from the index.
		if (must_alpha_hide(term, index_map))
		{
			hidden_map = alpha_hide(term, index_map);

			// If the hidden map is empty, then there is no more
			// substitution to be done.
			if (hidden_map.empty())
				return term;

			// Otherwise the new map must be passed down below
			index_map_ptr = &hidden_map;
		}
	}

	// Recursively fill out the subtrees.
	HandleSeq oset;
	bool changed = false;
	for (const Handle& h : term->getOutgoingSet())
	{
		// GlobNodes are matched with a list of one or more arguments.
		// Those arguments need to be in-lined, stripping off the list
		// that wraps them up.  See FilterLinkUTest for examples.
		if (GLOB_NODE == h->get_type())
		{
			Handle glst(substitute_scoped(h, args, *index_map_ptr, do_exec, quotation));
			changed = true;

			// Also unwrap any ListLinks that were inserted by
			// `wrap_glob_with_list()` in RewriteLink.cc
			if (glst->get_type() == LIST_LINK)
				for (const Handle &gl : glst->getOutgoingSet())
					oset.emplace_back(gl);
			else
				oset.emplace_back(glst);
		}
		else
		{
			Handle sub(substitute_scoped(h, args, *index_map_ptr, do_exec, quotation));
			if (sub != h)
			{
				changed = true;

				// End of the line for streaming data. If the arguments
				// that were being plugged in were executable streams,
				// but they're being placed into something that is not
				// executable, then run those streams, terminating them.
				// The final result is then just a plain-old ordinary
				// non-executable Atom, and nothing more.
				if (do_exec and
				    not term->is_executable() and
				    sub->is_executable())
				{
					ValuePtr evp = sub->execute();
					if (evp->is_atom())
						sub = HandleCast(evp);
					else
						throw InvalidParamterException(TRACE_INFO,
							"Wanted Atom as result of execution, got %s!"
							evp->to_string().c_str());
				}
			}
			oset.emplace_back(sub);
		}
	}

	// Return the original atom, if it was not modified.
	if (not changed) return term;
	return createLink(std::move(oset), term->get_type());
}

/* ================================================================= */

bool Replacement::must_alpha_convert(const Handle& scope,
                                     const HandleSeq& args)
{
	const HandleSet& vars = ScopeLinkCast(scope)->get_variables().varset;
	for (const Handle& value : args)
		if (vars.find(value) != vars.end())
			return true;
	return false;
}

// Evaluate whether any variable must be hidden/removed from the
// index_map.
bool Replacement::must_alpha_hide(const Handle& scope,
                                  const IndexMap& index_map)
{
	const HandleSet& vars = ScopeLinkCast(scope)->get_variables().varset;
	for (const Handle& v : vars)
		if (index_map.find(v) != index_map.end())
			return true;
	return false;
}

/* ================================================================= */

/// Remove the variables from the given index map that are present in
/// the given the variables of a scope, as well as non variables
Replacement::IndexMap Replacement::alpha_hide(const Handle& scope,
                                              const IndexMap& index_map)
{
	// Make a copy... this is what's computationally expensive.
	IndexMap hidden_map = index_map;

	// Remove the alpha-hidden variables.
	const HandleSet& vars = ScopeLinkCast(scope)->get_variables().varset;
	for (const Handle& v : vars)
	{
		IndexMap::const_iterator idx = hidden_map.find(v);
		if (idx != hidden_map.end())
		{
			hidden_map.erase(idx);
		}
	}

	// Also remove everything that is not a variable.
	// The map will, in general, contain terms that
	// contain alpha-hidden variables; those also have
	// to go, or they will mess up the substitution.
	for (auto it = hidden_map.begin(); it != hidden_map.end();)
	{
		Type tt = it->first->get_type();
		if (tt != VARIABLE_NODE and tt != GLOB_NODE)
		{
			it = hidden_map.erase(it);
		}
		else
		{
			++it;
		}
	}

	return hidden_map;
}

/* ================================================================= */

} // ~namespace opencog
