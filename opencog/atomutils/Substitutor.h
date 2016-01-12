/*
 * Substitutor.h
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: William Ma <https://github.com/williampma>
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

#ifndef _OPENCOG_SUBSTITUTOR
#define _OPENCOG_SUBSTITUTOR


#include <opencog/atoms/base/Link.h>


namespace opencog
{

/**
 * Given a term, substitute one set of atoms for another within that
 * term.  That is, given a substituion map from atoms to atoms, then,
 * if the term contains a substitutable atom in it, then the
 * substutition will be made. The entire term will be searched, i.e.
 * recursively downwards. However, only one level of substitution is
 * performed; that is, atoms in the substituted terms won't be
 * substituted.
 *
 * Similar to the Instantiator class, but does not execute stuff;
 * also can substitute non-VariableNode atoms.
 *
 * Similar to VariableList's substitute() method, but works for
 * non-VariableNode atoms.
 */
class Substitutor
{
private:

	// Placing vmap first allows the compiler to optimize the stack
	// frame. That is, expr changes each time, but vmap does not.
	static Handle walk_tree(const std::map<Handle, Handle> &vmap, const Handle& expr)
	{
		std::map<Handle,Handle>::const_iterator it = vmap.find(expr);
		if (vmap.end() != it )
			return it->second;

		LinkPtr lexpr(LinkCast(expr));

		// If not a link, and not mapped, just return it.
		if (not lexpr)
			return Handle(expr);

		HandleSeq oset_results;
		bool changed = false;
		for (const Handle& h : lexpr->getOutgoingSet())
		{
			Handle hg = walk_tree(vmap, h);
			if (hg != h) changed = true;
			oset_results.emplace_back(hg);
		}

		if (not changed) return expr;

		// Create a duplicate link with the substitution.
		return Handle(createLink(expr->getType(), oset_results,
		                         expr->getTruthValue()));
	}

public:
	/**
	 * The main method to call to substitue sub-atoms.
	 *
	 * @param expr  the original atom
	 * @param vars  an atom to atom mapping
	 * @return      a new atom with sub-atoms replaced
	 */
	static Handle substitute(const Handle& expr,
	                         const std::map<Handle, Handle> &vars)
	{
		// throw, not assert, because this is a user error ...
		if (nullptr == expr)
			throw InvalidParamException(TRACE_INFO,
				"Asked to substitute a null expression");

		// The returned handle is not yet in the atomspace. Add it now.
		return  walk_tree(vars, expr);
	}
};

}

#endif // _OPENCOG_SUBSTITUTOR

