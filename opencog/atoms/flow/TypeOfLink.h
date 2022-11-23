/*
 * opencog/atoms/flow/TypeOfLink.h
 *
 * Copyright (C) 2015, 2022 Linas Vepstas
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

#ifndef _OPENCOG_TYPE_OF_LINK_H
#define _OPENCOG_TYPE_OF_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The TypeOfLink returns a vector of Types of the wrapped
/// atoms.
///
/// For example,
///
///     TypeOfLink
///         SomeAtom
///         OtherAtom
///
/// will return
///
///     (LinkValue (TypeNode 'SomeAtom) (TypeNode 'OtherAtom))
///
class TypeOfLink : public FunctionLink
{
public:
	TypeOfLink(const HandleSeq&&, Type = TYPE_OF_LINK);
	TypeOfLink(const TypeOfLink&) = delete;
	TypeOfLink& operator=(const TypeOfLink&) = delete;

	// Return a pointer to the atom being specified.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(TypeOfLink)
#define createTypeOfLink CREATE_DECL(TypeOfLink)

/** @}*/
}

#endif // _OPENCOG_TYPE_OF_LINK_H
