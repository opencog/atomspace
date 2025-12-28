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

#include <opencog/atoms/scope/Context.h>

/**
 * class Instantiator -- create grounded expressions from ungrounded ones.
 * Given an ungrounded expression (i.e. an expression containing variables)
 * and a map between variables and ground terms, it will create a new
 * expression, with the ground terms substituted for the variables.
 * The final result will be placed in the provided AtomSpace.
 *
 * This also implements generic execution: any executable links are
 * executed as the variable substitution is performed.  In particular,
 * execution is implemented as instantiation, with an empty variable
 * map.
 *
 * See also: `Replacement::replace_nocheck()`, which is similar, except
 * except that no execution is performed, and the result is not placed
 * into any AtomSpace.
 */
namespace opencog {

class Instantiator
{
private:
	AtomSpace *_as;
	const GroundingMap& _varmap;

	/**
	 * Instatiator removes first level QuoteLinks and in such cases
	 * returns verbatim atoms. This is incorrect when the QuoteLink
	 * occurs in any scoped link (anything inheriting from ScopeLink,
	 * (e.g. MeetLink, QueryLink), since these handle QuoteLinks within
	 * their own scope. We must avoid damaging quotes for these atoms.
	 */
	Context _context;

	/** Avoid infinite recursion. */
	bool _halt;

	/** Non-printing throws */
	bool _silent;

public:
	Instantiator(AtomSpace*, const GroundingMap&);

	/**
	 * Recursively walk a tree starting with the root, plugging in
	 * variables from the `_varmap`, respecting quotations.
	 *
	 * That is, perform a beta-reduction (substitution of variables
	 * by their values).
	 *
	 * See also the related function VariableList::substitute(),
	 * which will simply perform a substitution.
	 * See also PutLink, which does substitution (beta reduction).
	 */
	Handle walk_tree(const Handle& tree);
};

ValuePtr instantiate(AtomSpace*,
                     const GroundingMap&,
                     const Handle& expr,
                     bool silent=false);

} // namespace opencog

#endif // _OPENCOG_INSTANTIATOR_H

