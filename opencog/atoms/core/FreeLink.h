/*
 * opencog/atoms/core/FreeLink.h
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

#ifndef _OPENCOG_FREE_LINK_H
#define _OPENCOG_FREE_LINK_H

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/FreeVariables.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The FreeLink records all of the free variables that occur within
 * (underneath) it, in traversal order. Those variables are placed in
 * sequential order in _varseq. An index is placed in _index. That is,
 * given a variable, its ordinal number is placed in _index.
 */
class FreeLink : public Link
{
protected:
	FreeVariables _vars;
	void init(void);

public:
	FreeLink(const HandleSeq&&, Type=FREE_LINK);
	FreeLink(const FreeLink&) = delete;
	FreeLink& operator=(const FreeLink&) = delete;
	virtual ~FreeLink() {}

	const FreeVariables& get_vars() const
	{ return  _vars; }

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(FreeLink)
#define createFreeLink std::make_shared<FreeLink>

/** @}*/
}

#endif // _OPENCOG_FREE_LINK_H
