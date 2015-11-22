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
	SleepLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	SleepLink(Link &l);

	// Return a pointer to the atom being specified.
	virtual Handle execute(AtomSpace* = NULL) const;
};

typedef std::shared_ptr<SleepLink> SleepLinkPtr;
static inline SleepLinkPtr SleepLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<SleepLink>(a); }
static inline SleepLinkPtr SleepLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<SleepLink>(a); }

// XXX temporary hack ...
#define createSleepLink std::make_shared<SleepLink>

/** @}*/
}

#endif // _OPENCOG_SLEEP_LINK_H
