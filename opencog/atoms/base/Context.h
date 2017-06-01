/*
 * opencog/atoms/base/Context.h
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

#include <boost/operators.hpp>

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/base/Quotation.h>
#include <opencog/atoms/core/Variables.h>

#include <string>
#include <list>

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
struct Context : public boost::totally_ordered<Context>
{
	typedef std::list<Variables> VariablesStack;

	// Default ctor
	Context(const Quotation& quotation=Quotation(),
	        const OrderedHandleSet& shadow=OrderedHandleSet(),
	        bool store_scope_variables=true,
	        const VariablesStack& scope_variables=VariablesStack());
	Context(bool store_scope_variables);

	// Quotation state
	Quotation quotation;

	// Set of shadowing variables
	OrderedHandleSet shadow;

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
	 * variable, that is unquoted and unshadowed.
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
bool ohs_content_eq(const OrderedHandleSet& lhs, const OrderedHandleSet& rhs);

// For gdb, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const Context& c);
	
/** @}*/
} // namespace opencog

#endif // _OPENCOG_QUOTATION_H
