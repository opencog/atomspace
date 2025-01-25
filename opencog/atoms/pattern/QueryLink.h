/*
 * opencog/atoms/pattern/QueryLink.h
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
#ifndef _OPENCOG_QUERY_LINK_H
#define _OPENCOG_QUERY_LINK_H

#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/value/ContainerValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class QueryLink : public PatternLink
{
protected:
	void init(void);

	virtual ContainerValuePtr do_execute(AtomSpace*, bool silent);

public:
	QueryLink(const HandleSeq&&, Type=QUERY_LINK);
	QueryLink(const Handle& vardecl, const Handle& body, const Handle& rewrite);
	QueryLink(const Handle& body, const Handle& rewrite);

	QueryLink(const QueryLink&) = delete;
	QueryLink& operator=(const QueryLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(QueryLink)
#define createQueryLink CREATE_DECL(QueryLink)

/** @}*/
}

#endif // _OPENCOG_QUERY_LINK_H
