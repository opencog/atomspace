/*
 * opencog/atoms/UniqueLink.h
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

#ifndef _OPENCOG_UNIQUE_LINK_H
#define _OPENCOG_UNIQUE_LINK_H

#include <opencog/atoms/core/FreeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The UniqueLink is used to guarantee uniqueness of a relationship
/// between the first Atom in the outgoing set, and the rest of the
/// outgoing set. This uniqueness constraint is enforced at the time
/// of AtomSpace insertion. Thus, only ONE UniqueLink, with a given
/// first-Atom, can exist in a AtomSpace at a time.
///
/// The uniqueness constraint is thread-safe: in general, the first
/// such UniqueLink "wins", and blocks subsequnt ones. Thus, they can
/// be used to build more complex thread-safe Link types.
///
/// This class is intended to be the base class for GrantLink, which
/// is used to issue unique names for things, the DefineLink, which
/// is used to define procedures, predicates and schemas, and StateLink,
/// which is used to maintain current state. It is also used by
/// TypedAtomLink to ensure that an atom, if it is typed, has a single,
/// unique type definition.
///
/// This is a "private" Link type, means as a building block for other
/// Link types that enforce different kinds of uniqueness semantics.
/// This includes mutatibility within a single AtomSpace (StateLinks
/// are mutable, DefineLinks and GrantLinks are not) and mutability
/// via Frames (StateLinks and DefineLinks in deeper Frames can be
/// hidden by shallower StateLinks and DefineLinks; however, GrantLinks
/// are the same in all frames.)
///
class UniqueLink : public FreeLink
{
protected:
	void init(bool);

	static Handle get_unique_nt(const Handle&, Type, bool, const AtomSpace*);
	static Handle get_unique(const Handle&, Type, bool, const AtomSpace*);

public:
	UniqueLink(const HandleSeq&&, Type=UNIQUE_LINK);
	UniqueLink(const Handle& alias, const Handle& body);

	UniqueLink(const UniqueLink&) = delete;
	UniqueLink& operator=(const UniqueLink&) = delete;

	Handle get_alias(void) const { return _outgoing.at(0); }

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(UniqueLink)
#define createUniqueLink CREATE_DECL(UniqueLink)

/** @}*/
}

#endif // _OPENCOG_UNIQUE_LINK_H
