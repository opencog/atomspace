/*
 * opencog/atoms/flow/KeysOfLink.h
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

#ifndef _OPENCOG_KEYS_OF_LINK_H
#define _OPENCOG_KEYS_OF_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The KeysOfLink returns a LinkValue holding all of the keys
/// that are in use on the atoms in the outgoing set.
///
/// For example,
///
///     KeysOfLink
///         Concept "foo"
///         Concept "bar"
///
/// will return
///
///     (LinkValue (Predicate "key1") (Predicate "key2") ...)
///
/// containing all keys from both atoms, merged into one set.
///
class KeysOfLink : public FunctionLink
{
public:
	KeysOfLink(const HandleSeq&&, Type = KEYS_OF_LINK);
	KeysOfLink(const KeysOfLink&) = delete;
	KeysOfLink& operator=(const KeysOfLink&) = delete;

	// Return a pointer to LinkValue holding all the keys.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(KeysOfLink)
#define createKeysOfLink CREATE_DECL(KeysOfLink)

/** @}*/
}

#endif // _OPENCOG_KEYS_OF_LINK_H
