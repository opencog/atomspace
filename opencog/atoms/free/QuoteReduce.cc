/*
 * QuoteReduce.cc
 *
 * Copyright (C) 2009, 2014, 2015, 2025 Linas Vepstas
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
 */

#include <opencog/atoms/atom_types/atom_types.h>
#include "QuoteReduce.h"

using namespace opencog;

QuoteReduce::QuoteReduce(const GroundingMap& varmap) :
	_varmap(varmap),
	_context(false),
	_halt(false),
	_silent(false)
{}

/// walk_tree() performs beta-reduction, respecting the use of
/// quotations and quotation contexts. This is a hack, because
/// of a combination of two things: QuoteLink is mis-designed,
/// and beta-reduction should have respected quote link. So this
/// is here, for now.
Handle QuoteReduce::walk_tree(const Handle& expr)
{
	Type t = expr->get_type();

	// Store the current context so we can update it for subsequent
	// recursive calls of walk_tree.
	Context context_cp(_context);
	_context.update(expr);

	// Discard the following QuoteLink, UnquoteLink or LocalQuoteLink
	// as it is serving its quoting or unquoting function.
	if (context_cp.consumable(t))
	{
		if (1 != expr->get_arity())
			throw InvalidParamException(TRACE_INFO,
			                            "QuoteLink/UnquoteLink has "
			                            "unexpected arity!");
		Handle child = expr->getOutgoingAtom(0);
		return walk_tree(child);
	}

	if (expr->is_node())
	{
		if (context_cp.is_quoted())
			return expr;

		if (VARIABLE_NODE != t and GLOB_NODE != t)
			return expr;

		// If it is a quoted or shadowed variable don't substitute.
		// TODO: what about globs?
		if (VARIABLE_NODE == t and not context_cp.is_free_variable(expr))
			return expr;

		// If we are here, we found a free variable or glob. Look
		// it up. Return a grounding if it has one, otherwise return
		// the variable itself.
		GroundingMap::const_iterator it = _varmap.find(expr);
		if (_varmap.end() == it) return expr;

		// Halt infinite regress. This can happen when the Variable
		// has a grounding that contains Variables ... etc.
		if (_halt)
			return expr;

		_halt = true;
		Handle hgnd(walk_tree(it->second));
		_halt = false;
		return hgnd;
	}

	// -----------------------------------------------------------
	// If we are here, then we have a link. Walk it. In general,
	// links may contain both bound variables, and also free variables.
	// We must be careful to substitute only for free variables, and
	// never for bound ones.

	HandleSeq oset_results;
	bool changed = false;
	Context save_context = _context;
	for (const Handle& h : expr->getOutgoingSet())
	{
		Handle hg(walk_tree(h));
		_context = save_context;
		if (hg != h) changed = true;

		// GlobNodes are grounded by a ListLink of everything that
		// the GlobNode matches. Unwrap the list, and insert each
		// of the glob elements in sequence.
		Type ht = h->get_type();
		if (changed and
		    ((_context.is_unquoted() and GLOB_NODE == ht) or
		    ((UNQUOTE_LINK == ht and
		      GLOB_NODE == h->getOutgoingAtom(0)->get_type()))))
		{
			for (const Handle& gloe: hg->getOutgoingSet())
			{
				if (NULL != gloe)
					oset_results.emplace_back(gloe);
			}
		}
		else
		{
			// It could be a NULL handle if it's deleted...
			// Just skip over it.
			if (hg) oset_results.emplace_back(hg);
		}
	}
	if (not changed)
		return expr;

	return createLink(std::move(oset_results), t);
}

/* ===================== END OF FILE ===================== */
