/*
 * opencog/atoms/pattern/MeetLink.h
 *
 * Copyright (C) 2019 Linas Vepstas
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
#ifndef _OPENCOG_MEET_LINK_H
#define _OPENCOG_MEET_LINK_H

#include <opencog/atoms/pattern/PatternLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class MeetLink : public PatternLink
{
protected:
	void init(void);
	virtual QueueValuePtr do_execute(AtomSpace*, bool silent);

public:
	MeetLink(const HandleSeq&&, Type=MEET_LINK);

	MeetLink(const MeetLink&) = delete;
	MeetLink operator=(const MeetLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(MeetLink)
#define createMeetLink std::make_shared<MeetLink>

/** @}*/
}

#endif // _OPENCOG_MEET_LINK_H
