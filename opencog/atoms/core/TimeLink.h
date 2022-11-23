/*
 * opencog/atoms/core/TimeLink.h
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

#ifndef _OPENCOG_TIME_LINK_H
#define _OPENCOG_TIME_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The TimeLink returns a FloatValue holding the current time.
/// At this time, it takes no arguments.
///
class TimeLink : public FunctionLink
{
public:
	TimeLink(const HandleSeq&&, Type=TIME_LINK);
	TimeLink(const TimeLink&) = delete;
	TimeLink& operator=(const TimeLink&) = delete;

	// Return a pointer to the atom being specified.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(TimeLink)
#define createTimeLink CREATE_DECL(TimeLink)

/** @}*/
}

#endif // _OPENCOG_TIME_LINK_H
