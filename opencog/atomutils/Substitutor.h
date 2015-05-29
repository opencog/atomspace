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


#include <opencog/atomspace/AtomSpace.h>


namespace opencog
{

/**
 * Like the Instantiator but does not execute stuff, and also
 * works for non-variable.
 */
class Substitutor
{
private:
	AtomSpace *_as;
	const std::map<Handle, Handle> *_vmap;

	Handle walk_tree(const Handle& expr)
	{
		std::map<Handle,Handle>::const_iterator it = _vmap->find(expr);
		if (_vmap->end() != it )
			return it->second;

		LinkPtr lexpr(LinkCast(expr));

		// if not a link, and not mapped, just return it
		if (not lexpr)
			return Handle(expr);

		HandleSeq oset_results;
		for (const Handle& h : lexpr->getOutgoingSet())
		{
			Handle hg = walk_tree(h);
			oset_results.push_back(hg);
		}

		// Now create a duplicate link with the substitution
		return Handle(createLink(expr->getType(), oset_results, expr->getTruthValue()));
	}

public:
	Substitutor(AtomSpace* as) : _as(as) {}

	Handle substitute(const Handle& expr, const std::map<Handle, Handle> &vars)
	{
		// throw, not assert, because this is a user error ...
		if (Handle::UNDEFINED == expr)
			throw InvalidParamException(TRACE_INFO,
				"Asked to substitute a null expression");

		_vmap = &vars;

		// The returned handle is not yet in the atomspace. Add it now.
		Handle hn = walk_tree(expr);
		if (NULL != hn)
			return _as->addAtom(hn);
		return hn;
	}
};

}

#endif // _OPENCOG_SUBSTITUTOR

