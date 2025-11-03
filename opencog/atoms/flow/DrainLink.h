/*
 * opencog/atoms/flow/DrainLink.h
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

#ifndef _OPENCOG_DRAIN_LINK_H
#define _OPENCOG_DRAIN_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The DrainLink exhausts a stream by repeatedly pulling from it
/// until the stream is empty, discarding all values.
///
/// For example,
///
///     DrainLink
///         SomeStreamSource
///
/// will pull all values from SomeStreamSource and discard them.
///
class DrainLink : public Link
{
public:
	DrainLink(const HandleSeq&&, Type = DRAIN_LINK);
	DrainLink(const DrainLink&) = delete;
	DrainLink& operator=(const DrainLink&) = delete;

	// Return a pointer to a Value.
	virtual ValuePtr execute(AtomSpace*, bool);
	virtual bool is_executable() const { return true; }

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(DrainLink)
#define createDrainLink CREATE_DECL(DrainLink)

/** @}*/
}

#endif // _OPENCOG_DRAIN_LINK_H
