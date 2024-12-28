/*
 * opencog/atoms/flow/SplitLink.h
 *
 * Copyright (C) 2015, 2022, 2024 Linas Vepstas
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

#ifndef _OPENCOG_SPLIT_LINK_H
#define _OPENCOG_SPLIT_LINK_H

#include <opencog/atoms/flow/CollectionOfLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The SplitLink splits SringValues (or Node names) aaccording
/// to whitespace, returning a LinkValue of the split name(s).
///
class SplitLink : public CollectionOfLink
{
public:
	SplitLink(const HandleSeq&&, Type = SPLIT_LINK);
	SplitLink(const SplitLink&) = delete;
	SplitLink& operator=(const SplitLink&) = delete;

	// Return a LinkValue
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(SplitLink)
#define createSplitLink CREATE_DECL(SplitLink)

/** @}*/
}

#endif // _OPENCOG_SPLIT_LINK_H
