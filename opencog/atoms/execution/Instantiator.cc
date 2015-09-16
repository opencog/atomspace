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

#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/core/PutLink.h>
#include <opencog/atoms/execution/ExecutionOutputLink.h>
#include <opencog/atoms/reduct/FoldLink.h>
#include <opencog/query/BindLinkAPI.h>

#include "Instantiator.h"

using namespace opencog;

Handle Instantiator::walk_tree(const Handle& expr)
{
	Type t = expr->getType();

	// Must not explore the insides of a QuoteLink.
	// XXX TODO: Need to implement UNQUOTE_LINK here...
	if (QUOTE_LINK == t)
		return Handle(expr);

	LinkPtr lexpr(LinkCast(expr));
	if (not lexpr)
	{
		// If we are here, we are a Node.
		if (DEFINED_SCHEMA_NODE == t)
		{
			return walk_tree(DefineLink::get_definition(expr));
		}

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

		// halt infinite regress
		if (_halt)
			return Handle(expr);

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
	//
	// Reduce PutLinks.
	if (PUT_LINK == t)
	{
		PutLinkPtr ppp(PutLinkCast(expr));

		// PutLinks always have arity two. There may be free vars in
		// the body of the PutLink, but we won't substitue for them until
		// after the beta-reduction.  Do substitute the free vars that
		// occur in the argument, and do that before beta-reduction.
		// Execute the body only after the beta-reduction has been done.
		const HandleSeq& oset = lexpr->getOutgoingSet();
		Handle gargs = walk_tree(oset[1]);
		if (gargs != oset[1])
		{
			HandleSeq groset;
			groset.push_back(oset[0]);
			groset.push_back(gargs);
			ppp = createPutLink(groset);
		}
		// Step one: beta-reduce.
		Handle red = ppp->reduce();
		// Step two: execute the resulting body.
		return walk_tree(red);
	}

	// If we are here, we assume that any/all variables that are in
	// the outgoing set are free variables. Substitute the ground
	// values for them. Do this by tree-walk.
	HandleSeq oset_results;
	for (const Handle& h : lexpr->getOutgoingSet())
	{
		Handle hg = walk_tree(h);
		// It could be a NULL handle if it's deleted... Just skip
		// over it. We test the pointer here, not the uuid, since
		// the uuid's are all Handle::UNDEFINED until we put them
		// into the atomspace.
		if (NULL != hg)
			oset_results.push_back(hg);
	}

	// If there is a GetLink, we have to perform the get, and replace
	// it with the results of the get. The get is implemented with the
	// PatternLink::satisfy() method.
	if (GET_LINK == t)
	{
		size_t sz = oset_results.size();
		for (size_t i=0; i< sz; i++)
			oset_results[i] = _as->add_atom(oset_results[i]);

		LinkPtr lp = createLink(GET_LINK, oset_results);

		return satisfying_set(_as, Handle(lp));
	}

	// Handle DeleteLink's before general FunctionLink's; they
	// work differently.
	if (DELETE_LINK == t)
	{
		for (const Handle& h: oset_results)
			if (VARIABLE_NODE != h->getType())
				_as->remove_atom(h, true);

		return Handle::UNDEFINED;
	}

	// ExecutionOutputLinks are not handled by the FunctionLink factory
	// below. This is due to a circular shared-libarary dependency.
	if (EXECUTION_OUTPUT_LINK == t)
	{
		// The atoms being created above might not all be in the
		// atomspace, just yet. Because we have no clue what the
		// ExecutionOutputLink might do (its a black box), we had
		// best put them there now. In particular, the black box
		// may want to look at the atom TV's, and for that, they
		// MUST be fetched from the atomspace, since only the
		// atomspace knows the correct TV values.
		//
		// The problem here is that the insertion leaves garbage
		// intermediate-result atoms littering the atomspace, with
		// no effective way of removing them. XXX This needs fixing.
		// Again, some kind of monad solution. XXX FIXME later.
		//
		// Just as well, because it seems the scheme (and python)
		// bindings get tripped up by the UUID==-1 of uninserted atoms.
		// XXX Well, this arguably means that scheme and python are
		// broken.
		size_t sz = oset_results.size();
		for (size_t i=0; i< sz; i++)
			oset_results[i] = _as->add_atom(oset_results[i]);

		ExecutionOutputLinkPtr eolp(createExecutionOutputLink(oset_results));
		return eolp->execute(_as);
	}

	// Fire execution links, if found.
	if (classserver().isA(t, FUNCTION_LINK))
	{
		// FoldLink's cannot be handled by the factory below, due to
		// ciruclar shared library dependencies. Yuck. Somethine better
		// than a factory needs to be invented.
		if (classserver().isA(t, FOLD_LINK))
		{
			Handle hl(FoldLink::factory(t, oset_results));
			FoldLinkPtr flp(FoldLinkCast(hl));
			return flp->execute(_as);
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
		return _as->add_atom(gnd);
	return gnd;
}

/* ===================== END OF FILE ===================== */
