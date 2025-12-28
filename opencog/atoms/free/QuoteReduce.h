/*
 * QuoteReduce.h
 *
 * Copyright (C) 2009, 2014, 2025 Linas Vepstas
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

#ifndef _OPENCOG_QUOTE_REDUCE_H
#define _OPENCOG_QUOTE_REDUCE_H

#include <opencog/atoms/free/Context.h>

/**
 * class QuoteReduce -- create grounded expressions from ungrounded
 * ones, respecting quotations.
 *
 * Given an ungrounded expression (i.e. an expression containing variables)
 * and a map between variables and ground terms, it will create a new
 * expression, with the ground terms substituted for the variables.
 *
 * See also: `Replacement::replace_nocheck()`, which is similar, except
 * except that it does not respect quotation.
 *
 * See also the related function VariableList::substitute(),
 * which will simply perform a substitution.
 *
 * See also PutLink, which does substitution (beta reduction).
 */
namespace opencog {

class QuoteReduce
{
private:
	const GroundingMap& _varmap;

	/**
	 * Reduction removes first level QuoteLinks and in such cases
	 * returns verbatim atoms. This is incorrect when the QuoteLink
	 * occurs in any scoped link (anything inheriting from ScopeLink,
	 * (e.g. MeetLink, QueryLink), since these handle QuoteLinks within
	 * their own scope. We must avoid damaging quotes for these atoms.
	 * (??? What ???)
	 */
	Context _context;

	/** Avoid infinite recursion. */
	bool _halt;

	/** Non-printing throws */
	bool _silent;

public:
	QuoteReduce(const GroundingMap&);

	/**
	 * Recursively walk a tree starting with the root, plugging in
	 * variables from the `_varmap`, respecting quotations.
	 */
	Handle walk_tree(const Handle& tree);
};

} // namespace opencog

#endif // _OPENCOG_QUOTE_REDUCE_H

