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

/// The UniqueLink is used to force uniqueness of a link from a given
/// atom, appearing as the first member of the outgoing set, to the
/// rest of the outgoing set.  Any attempt to make another UniqueLink
/// with the same atom in the first outgoing slot will throw an error.
/// Thus, only ONE UniqueLink with a given first-atom can exist at a
/// time.
///
/// This class is intended to be the base class for DefineLink, which
/// is used to name things, and StateLink, which is used to maintain
/// current state. It is also used by TypedAtomLink to ensure that
/// an atom, if it is typed, has a single, unique type definition.
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

typedef std::shared_ptr<UniqueLink> UniqueLinkPtr;
static inline UniqueLinkPtr UniqueLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<UniqueLink>(h); }
static inline UniqueLinkPtr UniqueLinkCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<UniqueLink>(a); }

#define createUniqueLink std::make_shared<UniqueLink>

/** @}*/
}

#endif // _OPENCOG_UNIQUE_LINK_H
