/*
 * opencog/atoms/constrain/IsClosedLink.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics LLC
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

#ifndef _OPENCOG_IS_CLOSED_LINK_H
#define _OPENCOG_IS_CLOSED_LINK_H

#include <opencog/atoms/execution/EvaluatableLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The IsClosedLink checks whether all of its arguments are closed,
 * i.e. whether they contain no free variables (VariableNodes or GlobNodes).
 *
 * Returns true if all arguments are closed (ground terms),
 * false otherwise.
 */
class IsClosedLink : public EvaluatableLink
{
public:
	IsClosedLink(const HandleSeq&&, Type=IS_CLOSED_LINK);
	IsClosedLink(const IsClosedLink&) = delete;
	IsClosedLink& operator=(const IsClosedLink&) = delete;
	virtual ~IsClosedLink() {}

	virtual bool bevaluate(AtomSpace*, bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(IsClosedLink)
#define createIsClosedLink CREATE_DECL(IsClosedLink)

/** @}*/
}

#endif // _OPENCOG_IS_CLOSED_LINK_H
