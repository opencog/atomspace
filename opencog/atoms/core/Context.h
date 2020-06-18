/*
 * opencog/atoms/core/Context.h
 *
 * Copyright (C) 2017 OpenCog Foundation
 * All Rights Reserved
 *
 * Written by Nil Geisweiller
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

#ifndef _OPENCOG_CONTEXT_H
#define _OPENCOG_CONTEXT_H

#include <list>
#include <string>

#include <opencog/util/empty_string.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atoms/core/Variables.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * A context holds the quotation state and the current shadowing
 * variables of a atom (typically coming from ancestor scopes).
 *
 * The context is important to have for both unification, in
 * particular sub-unification, see Unify::subunify, and closure, see
 * Unify::substitution_closure, because quoted or shadowed variables
 * should not be substituted.
 *
 * This notion of context is distinct and unrelated to ContextLink.
 */
struct Context
{
	typedef std::list<Variables> VariablesStack;

	// Default ctor
	Context(const Quotation& quotation=Quotation(),
	        const HandleSet& shadow=HandleSet(),
	        bool store_scope_variables=true,
	        const VariablesStack& scope_variables=VariablesStack());
	Context(bool store_scope_variables);

	// Quotation state
	Quotation quotation;

	// Set of shadowing variables
	HandleSet shadow;

	// Flag to ignore pushing scope variables to avoid that cost when
	// not necessary
	bool store_scope_variables;

	// Stack of variable declarations corresponding to each ancestor
	// unquoted scopes, pushed in encountering order from the current
	// handle to the root.
	VariablesStack scope_variables;

	/**
	 * Update the context over an atom. That is if the atom is a
	 * consumable quotation then update the context quotation. If
	 * the atom is a scope link then update the context shadow.
	 */
	void update(const Handle& h);

	/**
	 * Short hands for usual Quotation class methods. Note all
	 * Quotations methods are here, feel free to add as convenient.
	 */
	bool is_quoted() const;
	bool is_unquoted() const;
	bool consumable(Type t) const;

	/**
	 * Return true iff the given atom in that context is a free
	 * variable (VariableNode or GlobNode), that is unquoted and
	 * unshadowed.
	 */
	bool is_free_variable(const Handle& h) const;

	/**
	 * Comparison.
	 */
	bool operator==(const Context& context) const;
	bool operator<(const Context& context) const;
};

// Compare by content instead of pointer. We probably want this to be
// the default, until then this function is here for that.
bool ohs_content_eq(const HandleSet& lhs, const HandleSet& rhs);

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string oc_to_string(const Context& c,
                         const std::string& indent=empty_string);
	
/** @}*/
} // namespace opencog

#endif // _OPENCOG_QUOTATION_H
