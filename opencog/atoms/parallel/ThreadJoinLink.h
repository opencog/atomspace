/*
 * opencog/atoms/parallel/ThreadJoinLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
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

#ifndef _OPENCOG_THREAD_JOIN_LINK_H
#define _OPENCOG_THREAD_JOIN_LINK_H

#include <opencog/atoms/parallel/ParallelLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class AtomSpace;

class ThreadJoinLink : public ParallelLink
{
public:
	ThreadJoinLink(const HandleSeq&&, Type=THREAD_JOIN_LINK);
	ThreadJoinLink(const ThreadJoinLink&) = delete;
	ThreadJoinLink& operator=(const ThreadJoinLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace* as, bool silent)
	{
		return execute(as, silent, as);
	}
	ValuePtr execute(AtomSpace*, bool, AtomSpace*);

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<ThreadJoinLink> ThreadJoinLinkPtr;
static inline ThreadJoinLinkPtr ThreadJoinLinkCast(const Handle& h)
   { AtomPtr a(h); return std::dynamic_pointer_cast<ThreadJoinLink>(a); }
static inline ThreadJoinLinkPtr ThreadJoinLinkCast(AtomPtr a)
   { return std::dynamic_pointer_cast<ThreadJoinLink>(a); }

#define createThreadJoinLink std::make_shared<ThreadJoinLink>

/** @}*/
}

#endif // _OPENCOG_THREAD_JOIN_LINK_H
