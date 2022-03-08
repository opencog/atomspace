/*
 * opencog/atoms/pattern/BindLink.h
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
#ifndef _OPENCOG_BIND_LINK_H
#define _OPENCOG_BIND_LINK_H

#include <opencog/atoms/pattern/QueryLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class BindLink : public QueryLink
{
protected:
	void init(void);

public:
	BindLink(const HandleSeq&&, Type=BIND_LINK);
	BindLink(const Handle& vardecl, const Handle& body, const Handle& rewrite);
	BindLink(const Handle& body, const Handle& rewrite);

	BindLink(const BindLink&) = delete;
	BindLink& operator=(const BindLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(BindLink)
#define createBindLink CREATE_DECL(BindLink)

/** @}*/
}

#endif // _OPENCOG_BIND_LINK_H
