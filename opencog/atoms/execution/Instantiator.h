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

	struct Instate
	{
		Instate(const GroundingMap& varmap) :
			_varmap(varmap),
			_context(false),
			_consume_quotations(true),
			_needless_quotation(true),
			_halt(false)
		{}
		const GroundingMap& _varmap;

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
		 * consuming quotes systematically may change the semantics of
		 * the program. This flag is here to properly control that.
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

		/** Avoid infinite recursion. */
		bool _halt;

		/** Non-printing throws */
		bool _silent;
	};

	/**
	 * Recursively walk a tree starting with the root, plugging in
	 * variables from the `_varmap`, and executing the resulting
	 * expression. Return the result of the execution.
	 *
	 * That is, perform a beta-reduction (substitution of variables
	 * by thier values) followed by execution of the resulting tree.
	 *
	 * See also the related function VariableList::substitute(),
	 * which will simply perform a substitution, without performing
	 * any execution. See also PutLink, which does substitution.
	 * (actually, beta reduction).
	 */
	Handle walk_tree(const Handle& tree,
	                 Instate&) const;
	bool walk_sequence(HandleSeq&, const HandleSeq&,
	                   Instate&) const;

	/// Substitute, but do not execute ExecutionOutputLinks
	Handle reduce_exout(const Handle& exout,
	                    Instate&) const;

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

	ValuePtr instantiate(const Handle& expr,
	                     const GroundingMap& vars,
	                     bool silent=false) const;

	ValuePtr execute(const Handle& expr, bool silent=false);
};

} // namespace opencog

#endif // _OPENCOG_INSTANTIATOR_H

