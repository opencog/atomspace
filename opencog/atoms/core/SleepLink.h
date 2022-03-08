/*
 * opencog/atoms/core/SleepLink.h
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

#ifndef _OPENCOG_SLEEP_LINK_H
#define _OPENCOG_SLEEP_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The SleepLink pauses execution for the given number of seconds.
/// If interrupted, it returns the number of seconds remaining.
/// At this time, there is no way to interrupt :-)
///
class SleepLink : public FunctionLink
{
public:
	SleepLink(const HandleSeq&&, Type=SLEEP_LINK);
	SleepLink(const SleepLink &) = delete;
	SleepLink& operator=(const SleepLink &) = delete;

	// Return number of seconds left to sleep.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(SleepLink)
#define createSleepLink CREATE_DECL(SleepLink)

/** @}*/
}

#endif // _OPENCOG_SLEEP_LINK_H
