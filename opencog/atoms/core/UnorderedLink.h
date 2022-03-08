/*
 * opencog/atoms/core/UnorderedLink.h
 *
 * Copyright (C) 2017 Linas Vepstas
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

#ifndef _OPENCOG_UNORDERED_LINK_H
#define _OPENCOG_UNORDERED_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The UnorderedLink eliminates differences in the order of the
/// outgoing set.  It does so by always sorting the outgoing set into
/// some arbitrary order.
///
/// For example,
///
///     UnorderedLink
///         SomeAtom
///         OtherAtom
///
/// is exactly the same as
///
///     UnorderedLink
///         OtherAtom
///         SomeAtom
///
/// The uniformization of ordering is done in the constructor.
///
class UnorderedLink : public Link
{
public:
	UnorderedLink(const HandleSeq&&, Type=UNORDERED_LINK);
	UnorderedLink(const HandleSet&, Type=UNORDERED_LINK);

	UnorderedLink(const UnorderedLink&) = delete;
	UnorderedLink& operator=(const UnorderedLink&) = delete;

	virtual bool is_unordered_link() const { return true; }

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(UnorderedLink)
#define createUnorderedLink CREATE_DECL(UnorderedLink)

/** @}*/
}

#endif // _OPENCOG_UNORDERED_LINK_H
