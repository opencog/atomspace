/*
 * opencog/atoms/core/TypedAtomLink.h
 *
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_TYPED_ATOM_LINK_H
#define _OPENCOG_TYPED_ATOM_LINK_H

#include <opencog/atoms/core/UniqueLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The TypedAtomLink is used to associate a type description to an
/// atom; typically to a GroundedSchemaNode, or GroundedPredicateNode,
/// but also to FunctionLinks or other atoms that might need to be
/// strictly typed.  The type description is (globally) unique, in that
/// there can only be one type definition for an atom.  Polymorphism,
/// if needed, can be accomplished with TypeChoiceLink.  Any attempt
/// to provide a different specification for an atom will throw an
/// error.  That is, only ONE TypedAtomLink can exist, at one time,
/// for a given atom.
///
/// Note that the mechanics of this link are nearly identical to that
/// of DefineLink.  However, the semantics is very different: the
/// DefineLink is used to give names to structures.  The TypedAtomLink
/// is used to classify structures into type-classes.
class TypedAtomLink : public UniqueLink
{
protected:
	void init();
public:
	TypedAtomLink(const HandleSeq&&, Type=TYPED_ATOM_LINK);
	TypedAtomLink(const Handle& alias, const Handle& body);

	TypedAtomLink(const TypedAtomLink&) = delete;
	TypedAtomLink& operator=(const TypedAtomLink&) = delete;

	Handle get_atom(void) const { return _outgoing.at(0); }
	Handle get_type(void) const { return _outgoing.at(1); }

	/**
	 * Given a Handle pointing to <atom> in
	 *
	 * TypedAtomLink
	 *    <atom>
	 *    <type-specification>
	 *
	 * return <type-specification>
	 */
	static Handle get_type(const Handle&, const AtomSpace*);

	/**
	 * Given a Handle pointing to <atom> in
	 *
	 * TypedAtomLink
	 *    <atom>
	 *    <type-specification>
	 *
	 * return the TypedAtomLink for the given AtomSpace.
	 */
	static Handle get_link(const Handle&, const AtomSpace*);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(TypedAtomLink)
#define createTypedAtomLink std::make_shared<TypedAtomLink>

/** @}*/
}

#endif // _OPENCOG_TYPED_ATOM_LINK_H
