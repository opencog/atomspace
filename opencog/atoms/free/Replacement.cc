/*
 * opencog/atoms/free/Replacement.cc
 *
 * Copyright (C) 2009, 2014, 2015, 2025  Linas Vepstas
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
 */

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/free/Context.h>
#include <opencog/atoms/scope/ScopeLink.h>

namespace opencog {

/* ================================================================= */

Handle Replacement::replace_nocheck(const Handle& term,
                                    const HandleMap& vm,
                                    bool do_exec)
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
	return substitute_scoped(term, to_insert, insert_index, do_exec, false);
}

/* ================================================================= */

/// Perform beta-reduction on the term.  This is more-or-less a purely
/// syntactic beta-reduction, except for three semantic parts:
///
/// 1. The semantics of QuoteLink, UnquoteLink is honoured, so that
///    quoted variables are not reduced.
///
/// 2. The semantics of scoping (alpha-conversion) is honored, so that
///    any scoped variables with the same name as the free variables
///    are alpha-hidden, possibly alpha-converted if the substituting
///    values are variables of the same name.
///
/// 3. Executable intermediates are executed. This is an important,
///    even central part of implementing flowing streams. However, it
///    might also be a design flaw that should be solved in some other
///    way. One key weirdness is that the intermediate results might
///    be executable atoms with wrapped Values (in ValueShimeLink).
///    These cannot be placed in any AtomSpace, and so have to be
///    executed "bare". This works, but... Hrmmmpf.
//
Handle Replacement::substitute_scoped(Handle term,
                                      const HandleSeq& args,
                                      const IndexMap& index_map,
                                      bool do_exec,
                                      const Context& context)
{
	bool unquoted = context.is_unquoted();

	// If we are not in a quote context, and `term` is a variable,
	// then just return the corresponding argument.
	if (unquoted)
	{
		IndexMap::const_iterator idx = index_map.find(term);
		if (idx != index_map.end())
		{
			// Substitute if variable is not a shadowing var inside a ScopeLink
			if (0 == context.shadow.count(term))
				return args.at(idx->second);
		}
	}

	// If its a node, and its not a variable, then it is a constant,
	// and just return that.
	if (not term->is_link()) return term;

	Type ty = term->get_type();

	// Update quotation for subsequent recursive calls
	Context updated_ctxt(context);
	updated_ctxt.quotation.update(ty);

	if (unquoted and nameserver().isA(ty, SCOPE_LINK))
	{
		// Perform alpha-conversion duck-n-cover.

		// If a substituting value happens to be a variable in a ScopeLink,
		// then alpha-convert the scope to avoid variable name
		// collision. Loop in the rare case the new names collide.
		while (must_alpha_convert(term, args))
			term = ScopeLinkCast(term)->alpha_convert();

		// Add this scope's variables to the shadow set.
		const Variables& variables = ScopeLinkCast(term)->get_variables();
		updated_ctxt.shadow.insert(variables.varset.begin(), variables.varset.end());
	}

	// Recursively fill out the subtrees.
	HandleSeq oset;
	bool changed = false;
	for (const Handle& h : term->getOutgoingSet())
	{
		Handle sub(substitute_scoped(h, args, index_map, do_exec, updated_ctxt));

		// GlobNodes are matched with a list of one or more arguments.
		// Those arguments need to be in-lined, stripping off the list
		// that wraps them up.  See FilterLinkUTest for examples.
		if (GLOB_NODE == h->get_type())
		{
			changed = true;

			// Also unwrap any ListLinks that were inserted by
			// `wrap_glob_with_list()` in RewriteLink.cc
			// (Unordered globs get wrapped with UnorderedLink)
			if (sub->get_type() == LIST_LINK or
			    sub->get_type() == UNORDERED_LINK)
				for (const Handle &gl : sub->getOutgoingSet())
					oset.emplace_back(gl);
			else
				oset.emplace_back(sub);

			continue;
		}

		if (sub != h)
		{
			changed = true;

			// End of the line for streaming data. If the arguments
			// that were being plugged in were executable streams,
			// but they're being placed into something that is not
			// executable, then run those streams, terminating them.
			// The final result is then just a plain-old ordinary
			// non-executable Atom, and nothing more.
			//
			// Well, sort-of. Execution could return something that
			// is not an Atom. In that case, we record the original
			// form. This original form might be executed again,
			// later on, and if this execution has side-effects,
			// then, well, things get ugly.  But there's no obvious
			// way of avoiding this; we'd need some method that
			// tells us if execution returns only Atoms and never
			// Values. And we don't have such a function...
			if (do_exec and
			    not term->is_executable() and
			    sub->is_executable())
			{
				// AtomSpace* as = term->getAtomSpace();
				ValuePtr evp = sub->execute();
				if (evp->is_atom())
					sub = HandleCast(evp);
			}
		}
		oset.emplace_back(sub);
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

/* ================================================================= */

} // ~namespace opencog
