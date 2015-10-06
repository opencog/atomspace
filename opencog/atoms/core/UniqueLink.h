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

#include <map>

#include <opencog/atomspace/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The UniqueLink is used to force uniqueness of a link, given the
/// first atom of the outgoing set.  Any attempt to make a different
/// definition with the same name will throw an error.  Thus, only
/// ONE UniqueLink with a given first-atom can exist at a time.
///
/// This class is intended to be the base class for DefineLink, which
/// is used to name things, and StateLink, which is used to maintain
/// current state.
///
class UniqueLink : public Link
{
protected:
	void init(Type);
	static Handle get_unique(const Handle&, Type);
public:
	UniqueLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	UniqueLink(const Handle& alias, const Handle& body,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	UniqueLink(Link &l);

	Handle get_alias(void) const { return _outgoing[0]; }
};

typedef std::shared_ptr<UniqueLink> UniqueLinkPtr;
static inline UniqueLinkPtr UniqueLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<UniqueLink>(a); }
static inline UniqueLinkPtr UniqueLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<UniqueLink>(a); }

// XXX temporary hack ...
#define createUniqueLink std::make_shared<UniqueLink>

/** @}*/
}

#endif // _OPENCOG_UNIQUE_LINK_H
