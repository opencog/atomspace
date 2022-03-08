/*
 * opencog/atoms/pattern/GetLink.h
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
#ifndef _OPENCOG_GET_LINK_H
#define _OPENCOG_GET_LINK_H

#include <opencog/atoms/pattern/MeetLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class GetLink : public MeetLink
{
protected:
	void init(void);

public:
	GetLink(const HandleSeq&&, Type=GET_LINK);

	GetLink(const GetLink&) = delete;
	GetLink operator=(const GetLink&) = delete;

	virtual ValuePtr execute(AtomSpace*, bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(GetLink)
#define createGetLink std::make_shared<GetLink>

/** @}*/
}

#endif // _OPENCOG_GET_LINK_H
