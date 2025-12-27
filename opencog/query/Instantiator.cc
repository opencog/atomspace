/*
 * Instantiator.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/grant/DefineLink.h>
#include <opencog/atoms/scope/LambdaLink.h>
#include <opencog/atoms/scope/PutLink.h>
#include <opencog/atoms/execution/ExecutionOutputLink.h>
#include <opencog/atoms/flow/ValueShimLink.h>

#include "Instantiator.h"

using namespace opencog;

Instantiator::Instantiator(AtomSpace* as) : _as(as)
{}

Instantiator::Instantiator(const AtomSpacePtr& asp) : _as(asp.get())
{}

/// Perform beta-reduction on the expression `expr`, using the `vmap`
/// to fish out values for variables.  The map holds pairs: the first
/// member of the pair is the variable; the second is the value that
/// should be used as its replacement.  (Note that "variables" do not
/// have to actually be VariableNode's; they can be any atom.)
static Handle beta_reduce(const Handle& expr, const GroundingMap& vmap)
{
	if (vmap.empty()) return expr;

	// Format conversion. FreeVariables::substitute_nocheck() performs
	// beta-reduction correctly, so we just use that. But we have to
	// jam the map into the format it expects.
	HandleSeq vals;
	FreeVariables crud;
	unsigned int idx = 0;
	for (const auto& pr : vmap)
	{
		crud.varseq.push_back(pr.first);
		crud.index.insert({pr.first, idx});
		vals.push_back(pr.second);
		idx++;
	}
	return crud.substitute_nocheck(expr, vals);
}

/// walk_tree() performs beta-reduction, respecting the use of
/// quotations and quotation contexts. This is a hack, because
/// of a combination of two things: QuoteLink is mis-designed,
/// and beta-reduction should have respected quote link. So this
/// is here, for now.
Handle Instantiator::walk_tree(const Handle& expr,
                               Instate& ist) const
{
	Type t = expr->get_type();

	// Store the current context so we can update it for subsequent
	// recursive calls of walk_tree.
	Context context_cp(ist._context);
	ist._context.update(expr);

	// Discard the following QuoteLink, UnquoteLink or LocalQuoteLink
	// as it is serving its quoting or unquoting function.
	if (context_cp.consumable(t))
	{
		if (1 != expr->get_arity())
			throw InvalidParamException(TRACE_INFO,
			                            "QuoteLink/UnquoteLink has "
			                            "unexpected arity!");
		Handle child = expr->getOutgoingAtom(0);
		return walk_tree(child, ist);
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
		GroundingMap::const_iterator it = ist._varmap.find(expr);
		if (ist._varmap.end() == it) return expr;

		// Not so fast, pardner. VariableNodes can be grounded by
		// links, and those links may be executable. In that case,
		// we have to execute them.

		// halt infinite regress
		if (ist._halt)
			return expr;

		ist._halt = true;
		Handle hgnd(walk_tree(it->second, ist));
		ist._halt = false;
		return hgnd;
	}

	// -----------------------------------------------------------
	// If we are here, then we have a link. Walk it. In general,
	// links may contain both bound variables, and also free variables.
	// We must be careful to substitute only for free variables, and
	// never for bound ones.

	HandleSeq oset_results;
	bool changed = false;
	Context cp_context = ist._context;
	for (const Handle& h : expr->getOutgoingSet())
	{
		Handle hg(walk_tree(h, ist));
		ist._context = cp_context;
		if (hg != h) changed = true;

		// GlobNodes are grounded by a ListLink of everything that
		// the GlobNode matches. Unwrap the list, and insert each
		// of the glob elements in sequence.
		Type ht = h->get_type();
		if (changed and
		    ((ist._context.is_unquoted() and GLOB_NODE == ht) or
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

	// Move over any Values hanging on the link.
	Handle subl(createLink(std::move(oset_results), t));
	subl->bulkCopyValues(expr);
	return subl;
}

/**
 * instantiate -- create a grounded expression from an ungrounded one.
 *
 * Given a handle to an ungrounded expression, and a set of groundings,
 * this will create a grounded expression.
 *
 * The set of groundings is to be passed in with the map 'vars', which
 * maps variable names to their groundings -- it maps variable names to
 * atoms that already exist in the atomspace.  This method will then go
 * through all of the variables in the expression, and substitute them
 * with their values, creating a new expression. The new expression is
 * added to the atomspace, and its handle is returned.
 */
ValuePtr Instantiator::instantiate(const Handle& expr,
                                   const GroundingMap& varmap,
                                   bool silent) const
{
	// throw, not assert, because this is a user error ...
	if (nullptr == expr)
		throw InvalidParamException(TRACE_INFO,
			"Asked to ground a null expression");

	Instate ist(varmap);
	ist._silent = silent;

	Type t = expr->get_type();

	// Execute any DefinedPredicateNodes
	if (nameserver().isA(t, DEFINED_PREDICATE_NODE) or
	    nameserver().isA(t, DEFINED_SCHEMA_NODE))
	{
		Handle defn(DefineLink::get_definition(expr));
		if (not defn->is_executable())
			return defn;
		return defn->execute(_as, silent);
	}

#if 1
	// Needed for AbsentUTest, DotLambdaTest, DotMashupTest.
	if (PUT_LINK == t)
	{
		// There are vars to be beta-reduced. Reduce them.
		ValuePtr reduced(beta_reduce(expr, ist._varmap));

		// (PutLink (DeleteLink ...)) returns nullptr
		if (nullptr == reduced) return nullptr;

		// Nothing more to do, if not an atom.
		if (not reduced->is_atom()) return reduced;

		Handle grounded(HandleCast(reduced));
		return grounded->execute(_as, silent);
	}
#endif

	// Instantiate.
	Handle grounded(walk_tree(expr, ist));

	// Fire any other executable links, not handled above.
	Type gt = grounded->get_type();
	if (nameserver().isA(gt, EXECUTABLE_LINK))
		return grounded->execute(_as, ist._silent);

	return grounded;
}

ValuePtr Instantiator::execute(const Handle& expr, bool silent)
{
	// Check for crazy cross-atomspace woes
	AtomSpace* exas = expr->getAtomSpace();
	if (nullptr != exas and not _as->in_environ(expr))
		throw RuntimeException(TRACE_INFO,
			"Can't execute: current AtomSpace is %s but atom is in AtomSpace %s",
			_as->get_name().c_str(), exas->get_name().c_str());

	// Expand on the spot.
	if (expr->is_type(DEFINED_SCHEMA_NODE))
	{
		Handle dex = DefineLink::get_definition(expr);
		if (dex->is_type(EXECUTABLE_LINK))
			return dex->execute(_as, silent);
	}

	// Try to execute directly, if possible.
	if (expr->is_executable())
		return expr->execute(_as, silent);

	return expr;
}

/* ===================== END OF FILE ===================== */
