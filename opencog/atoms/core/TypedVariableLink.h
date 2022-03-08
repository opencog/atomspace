/*
 * opencog/atoms/core/TypedVariableLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * All Rights Reserved
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

#ifndef _OPENCOG_TYPED_VARIABLE_LINK_H
#define _OPENCOG_TYPED_VARIABLE_LINK_H

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/TypeChoice.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The TypedVariableLink is used to attach a name to a type description;
/// the "name" is usually a VariableNode. Note that this is backwards
/// from the usual idea of attaching a type specification to a variable:
/// that is because we want to allow anonymous (unnamed) types to be
/// used, while, in certain special cases, we need to give names to
/// those types: the name is the VariableNode.
///
/// This does NOT inherit from DefineLink, because we allow the same
/// variable name to be used in diffent contexts to name completely
/// unrelated types.  Thus, the naming is strongly context-dependent.
///
/// The TypedVariableLink has the format:
/// ```
///     TypedVariableLink
///        <variable>
///        <type-specification>
/// ```
///
class TypedVariableLink : public Link
{
protected:
	void init();
	TypeChoicePtr _typech;

	ContentHash compute_hash() const;
public:
	TypedVariableLink(const HandleSeq&&, Type=TYPED_VARIABLE_LINK);
	TypedVariableLink(const Handle& alias, const Handle& body);

	TypedVariableLink(const TypedVariableLink&) = delete;
	TypedVariableLink& operator=(const TypedVariableLink&) = delete;

	Handle get_variable(void) const { return _outgoing.at(0); }
	TypeChoicePtr get_typedecl(void) const { return _typech; }

#if 1
	TypeSet get_simple_typeset(void) const
		{ return _typech->get_simple_typeset(); }
	HandleSet get_deep_typeset(void) const
		{ return _typech->get_deep_typeset(); }
	GlobInterval get_glob_interval(void) const 
		{ return _typech->get_glob_interval(); }

	bool is_globby(void) const
		{ return _typech->is_globby(); }
	bool is_lower_bound(size_t n) const
		{ return _typech->is_lower_bound(n); }
	bool is_upper_bound(size_t n) const
		{ return _typech->is_upper_bound(n); }

	bool is_type(const Handle& h) const
		{ return _typech->is_type(h); }
	bool is_type(Type t) const
		{ return _typech->is_type(t); }
#endif

	// The default interval for glob matching.
	const GlobInterval default_interval(void) const;

	bool is_untyped(void) const;

	bool is_equal(const TypedVariableLink&) const;
	bool operator==(const Atom&) const;

	std::string to_string(const std::string& indent) const;
	using Atom::to_string;

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(TypedVariableLink)
#define createTypedVariableLink CREATE_DECL(TypedVariableLink)

/** @}*/
}

#endif // _OPENCOG_TYPED_VARIABLE_LINK_H
