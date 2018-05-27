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

#include <opencog/atoms/core/Context.h>

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
	const HandleMap *_vmap;
	bool _halt;

	/**
	 * Instatiator removes first level QuoteLinks and in such cases
	 * returns verbatim atoms. This is incorrect when the QuoteLink
	 * occurs in any scoped link (anything inheriting from ScopeLink,
	 * (e.g. GetLink, BindLink), since these handle QuoteLinks within
	 * their own scope. We must avoid damaging quotes for these atoms.
	 */
	Context _context;

	/**
	 * Consuming quotation should only take place when this is called
	 * by a pattern matcher function, such as BindLink, GetLink and
	 * PutLink, etc, as part of the substitution mechanics. Otherwise,
	 * consuming quotes systematically may changes the semantics of
	 * the program. This flag is here properly control that.
	 */
	bool _consume_quotations;

	/**
	 * In case _consume_quotations is set to false, this can be set
	 * temporarily to false when consuming the quotation would change
	 * the semantics.
	 *
	 * TODO: maybe this can eliminate the need for
	 * _avoid_discarding_quotes_level
	 */
	bool _needless_quotation;

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
	 *
	 * There are two ways to do this: via eager execution, and via
	 * lazy execution. Lazy would be nicer, performance-wise, but this
	 * is still buggy, and unit tests will fail. So do eager execution
	 * by default.
	 */
	bool _eager;
	Handle walk_tree(const Handle& tree, bool silent=false);
	bool walk_sequence(HandleSeq&, const HandleSeq&, bool silent=false);

	/**
	 * Return true iff the following atom type may not match to
	 * itself, that is a scope, an evaluatable, used as logic
	 * connector by the pattern matcher, etc.
	 *
	 * TODO: should be refined to make the difference between AndLink
	 * as pattern matcher connector and AndLink as self-match.
	 *
	 * TODO: this should probably be moved to some pattern matcher
	 * method.
	 */
	static bool not_self_match(Type t);

public:
	Instantiator(AtomSpace* as);

	void ready(AtomSpace* as)
	{
		_as = as;
		_halt = false;
	}

	void clear()
	{
		_as = nullptr;
		_vmap = nullptr;
		_consume_quotations = true;
	}

	void reset_halt()
	{
		_halt = false;
	}

	// TODO: set consume_quotations to false when executing, set it to
	// true when instantiating
	ProtoAtomPtr instantiate(const Handle& expr, const HandleMap &vars,
	                   bool silent=false);
	ProtoAtomPtr execute(const Handle& expr, bool silent=false)
	{
		// If no actual instantiation is involved, then do not consume
		// quotations, as it might change the semantics. (??)
		_consume_quotations = false;
		return instantiate(expr, HandleMap(), silent);
	}
};

} // namespace opencog

#endif // _OPENCOG_INSTANTIATOR_H

