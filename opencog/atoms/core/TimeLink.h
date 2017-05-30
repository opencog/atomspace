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

/// The TimeLink returns a NumberNode holding the current time.
/// At this time, it takes no argumets.
///
class TimeLink : public FunctionLink
{
public:
	TimeLink(const HandleSeq&, Type=TIME_LINK);
	TimeLink(const Link&);

	// Return a pointer to the atom being specified.
	virtual Handle execute(AtomSpace* = NULL) const;

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<TimeLink> TimeLinkPtr;
static inline TimeLinkPtr TimeLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<TimeLink>(a); }
static inline TimeLinkPtr TimeLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<TimeLink>(a); }

// XXX temporary hack ...
#define createTimeLink std::make_shared<TimeLink>

/** @}*/
}

#endif // _OPENCOG_TIME_LINK_H
