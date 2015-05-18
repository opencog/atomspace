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

#include <opencog/atoms/reduct/FunctionLink.h>
#include <opencog/atoms/execution/ExecutionOutputLink.h>

#include "Instantiator.h"

using namespace opencog;

Handle Instantiator::walk_tree(const Handle& expr)
{
	// halt infinite regress
	if (_halt)
		return Handle(expr);

	Type t = expr->getType();

	// Must not explore the insides of a QuoteLink.
	if (QUOTE_LINK == t)
		return Handle(expr);

	LinkPtr lexpr(LinkCast(expr));
	if (not lexpr)
	{
		// If were are here, we are a Node.
		if (VARIABLE_NODE != t)
			return Handle(expr);

		// If we are here, we found a variable. Look it up. Return a
		// grounding if it has one, otherwise return the variable
		// itself.
		std::map<Handle,Handle>::const_iterator it = _vmap->find(expr);
		if (_vmap->end() == it) return Handle(expr);

		// Not so fast, pardner. VariableNodes can be grounded by
		// links, and those links may be executable. In that case,
		// we have to execute them.
		_halt = true;
		Handle hgnd(walk_tree(it->second));
		_halt = false;
		return hgnd;
	}

	// If we are here, then we have a link. Walk it.

	// Walk the subtree, substituting values for variables.
	HandleSeq oset_results;
	for (const Handle& h : lexpr->getOutgoingSet())
	{
		Handle hg = walk_tree(h);
		// It would be a NULL handle if it's deleted... Just skip
		// over it. We test the pointer here, not the uuid, since
		// the uuid's are all Handle::UNDEFINED until we put them
		// into the atomspace.
		if (NULL != hg)
			oset_results.push_back(hg);
	}

	if (DELETE_LINK == t)
	{
		for (Handle h: oset_results)
			_as->removeAtom(h, true);

		return Handle::UNDEFINED;
	}

	// Fire execution links, if found.
	if (classserver().isA(t, FUNCTION_LINK))
	{
		// The atoms being created above might not all be in the
		// atomspace, just yet. Because we have no clue what the
		// ExecutionOutputLink might do, we had best put them
		// there now. Just as well, because it seems the scheme
		// (and python) bindings get tripped up by the UUID==-1
		// of uninserted atoms.  XXX Well, this arguably means that
		// scheme and python are broken.  We shouldn't have to do
		// this, as it leaves garbage littered in the atomspace.
		// XXX FIXME later.
		size_t sz = oset_results.size();
		for (size_t i=0; i< sz; i++)
			oset_results[i] = _as->addAtom(oset_results[i]);

		// ExecutionOutputLinks are not handled by the factory below.
		// This is due to a circular shared-libarary dependency..
		if (EXECUTION_OUTPUT_LINK == t)
		{
			ExecutionOutputLinkPtr eolp(createExecutionOutputLink(oset_results));
			return eolp->execute(_as);
		}

		// This throws if it can't figure out the schema ...
		// Let the throw pass right on up the stack.
		Handle hl(FunctionLink::factory(t, oset_results));
		FunctionLinkPtr flp(FunctionLinkCast(hl));
		return flp->execute(_as);
	}

	// Now create a duplicate link, but with an outgoing set where
	// the variables have been substituted by their values.
	return Handle(createLink(t, oset_results, expr->getTruthValue()));
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
Handle Instantiator::instantiate(const Handle& expr,
                                 const std::map<Handle, Handle> &vars)
{
	// throw, not assert, because this is a user error ...
	if (Handle::UNDEFINED == expr)
		throw InvalidParamException(TRACE_INFO,
			"Asked to ground a null expression");

	_vmap = &vars;

	// The returned handle is not yet in the atomspace. Add it now.
	// We do this here, instead of in walk_tree(), because adding
	// atoms to the atomspace is an expensive process.  We can save
	// some time by doing it just once, right here, in one big batch.
	// A null pointer means that we hit
	Handle gnd = walk_tree(expr);
	if (NULL != gnd)
		return _as->addAtom(gnd);
	return gnd;
}

/* ===================== END OF FILE ===================== */
