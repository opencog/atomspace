/*
 * opencog/atoms/flow/AtomSpaceOfLink.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, Inc.
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

#ifndef _OPENCOG_ATOM_SPACE_OF_LINK_H
#define _OPENCOG_ATOM_SPACE_OF_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The AtomSpaceOfLink returns the AtomSpace that contains the
/// wrapped atom.
///
/// For example,
///
///     AtomSpaceOfLink
///         Concept "foo"
///
/// will return the AtomSpace containing the Concept "foo".
///
/// If the atom is not in any AtomSpace, a null handle is returned.
///
class AtomSpaceOfLink : public FunctionLink
{
public:
	AtomSpaceOfLink(const HandleSeq&&, Type = ATOM_SPACE_OF_LINK);
	AtomSpaceOfLink(const AtomSpaceOfLink&) = delete;
	AtomSpaceOfLink& operator=(const AtomSpaceOfLink&) = delete;

	// Return a Handle to the AtomSpace containing the atom.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(AtomSpaceOfLink)
#define createAtomSpaceOfLink CREATE_DECL(AtomSpaceOfLink)

/** @}*/
}

#endif // _OPENCOG_ATOM_SPACE_OF_LINK_H
