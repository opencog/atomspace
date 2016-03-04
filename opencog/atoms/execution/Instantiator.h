/*
 * Instantiator.h
 *
 * Copyright (C) 2009, 2014 Linas Vepstas
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

#ifndef _OPENCOG_INSTANTIATOR_H
#define _OPENCOG_INSTANTIATOR_H

#include <opencog/atomspace/AtomSpace.h>

/**
 * class Instantiator -- create grounded expressions from ungrounded ones.
 * Given an ungrounded expression (i.e. an expression containing variables)
 * and a map between variables and ground terms, it will create a new
 * expression, with the ground terms substituted for the variables.
 *
 * This also implements generic execution: any executable links are
 * executed as the variable substitution is performed.  In particular,
 * execution is implemented as instantiation, with an empty variable
 * map.
 */
namespace opencog {

class Instantiator
{
private:
	AtomSpace *_as;
	const std::map<Handle, Handle> *_vmap;
	bool _halt = false;
	int _quotation_level = 0;

	/**
	 * Instatiator removes first level QuoteLinks and in such cases
	 * returns verbatim atoms. This is sometimes incorrect if the first
	 * level QuoteLinks are placed below some kind of atoms e.g. GetLink
	 * or ExecutionOutputLink that take care of handling QuoteLinks
	 * on their own. We need to avoid discarding quotes for this atoms
	 * it will be done later according given atom type semantics.
	 */
	int _avoid_discarding_quotes_level = 0;

	/**
	 * Recursively walk a tree starting with the root of the
	 * hypergraph to instantiate (typically an ExecutionOutputLink).
	 *
	 * Return the current result of the execution. If the node is an
	 * ExecutionOutputLink then it returns the final result. If the
	 * node is another list (typically a ListLink) it returns a copy
	 * of it, replacing the variables in its outgoing by their
	 * respective groundings.
	 *
	 * See also the related function VariableList::substitute(),
	 * which will simply perform a substitution, without performing
	 * any execution. See also PutLink, which does substituion.
	 * (actually, beta reduction).
	 */
	Handle walk_eager(const Handle& tree);
	bool seq_eager(HandleSeq&, const HandleSeq& orig);

	/* Same as above, but does lazy execution. */
	Handle walk_lazy(const Handle& tree);
	bool seq_lazy(HandleSeq&, const HandleSeq& orig);

	bool walk_tree(HandleSeq&, const HandleSeq&,
	               Handle (Instantiator::*)(const Handle&));

public:
	Instantiator(AtomSpace* as) : _as(as) {}

	void ready(AtomSpace* as)
	{
		_as = as;
		_halt = false;
	}

	void clear()
	{
		_as = NULL;
		_vmap = NULL;
	}

	Handle instantiate(const Handle& expr, const std::map<Handle, Handle> &vars);
	Handle execute(const Handle& expr)
	{
			return instantiate(expr, std::map<Handle, Handle>());
	}
};

} // namespace opencog

#endif // _OPENCOG_INSTANTIATOR_H

