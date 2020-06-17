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

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The TypedVariableLink is used to attach a name to a type description;
/// the "name" is usually a VariableNode. Note that this is backwards
/// from the usual idea of attaching a type specification to a variable:
/// that is because we want to allow anonymous (unamed) types to be
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
	TypeSet _simple_typeset;
	HandleSet _deep_typeset;
	std::pair<size_t, size_t> _glob_interval;

	void init();
	void analyze();
public:
	TypedVariableLink(const HandleSeq&&, Type=TYPED_VARIABLE_LINK);
	TypedVariableLink(const Handle& alias, const Handle& body);

	TypedVariableLink(const TypedVariableLink&) = delete;
	TypedVariableLink& operator=(const TypedVariableLink&) = delete;

	Handle get_variable(void) const { return _outgoing.at(0); }
	Handle get_type(void) const { return _outgoing.at(1); }

	TypeSet get_simple_typeset(void) const { return _simple_typeset; }
	HandleSet get_deep_typeset(void) const { return _deep_typeset; }
	std::pair<size_t, size_t> get_glob_interval(void) const
		{ return _glob_interval; }

	// The default interval for glob matching.
	const std::pair<size_t, size_t> default_interval(void) const;

	bool is_globby(void) const;
	bool is_lower_bound(size_t) const;
	bool is_upper_bound(size_t) const;

	bool is_untyped(void) const;
	bool is_equal(const TypedVariableLink&) const;
	static Handle factory(const Handle&);
};

typedef std::shared_ptr<TypedVariableLink> TypedVariableLinkPtr;
static inline TypedVariableLinkPtr TypedVariableLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<TypedVariableLink>(h); }
static inline TypedVariableLinkPtr TypedVariableLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<TypedVariableLink>(a); }

#define createTypedVariableLink std::make_shared<TypedVariableLink>

/** @}*/
}

#endif // _OPENCOG_TYPED_VARIABLE_LINK_H
