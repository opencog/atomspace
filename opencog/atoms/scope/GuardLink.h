/*
 * opencog/atoms/scope/GuardLink.h
 *
 * Copyright (C) 2026 BrainyBlaize Dynamics LLC
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
 */

#ifndef _OPENCOG_GUARD_LINK_H
#define _OPENCOG_GUARD_LINK_H

#include <opencog/atoms/scope/ScopeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The GuardLink provides methods to determine if a proposed 
/// beta reduction is compatible with the argument types and
/// function body.
///
class GuardLink : public ScopeLink
{
public:
	GuardLink(const HandleSeq&&, Type=GUARD_LINK);
	GuardLink(const GuardLink &) = delete;
	GuardLink& operator=(const GuardLink &) = delete;

	bool guard(const HandleMap&) const;
	bool guard(const HandleSeq&) const;

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(GuardLink)
#define createGuardLink CREATE_DECL(GuardLink)

/** @}*/
}

#endif // _OPENCOG_GUARD_LINK_H
