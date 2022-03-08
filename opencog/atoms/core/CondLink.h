/*
 * opencog/atoms/core/CondLink.h
 *
 * Copyright (C) 2019 Kasim Ebrahim
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

#ifndef _OPENCOG_COND_LINK_H
#define _OPENCOG_COND_LINK_H

#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/core/ScopeLink.h>
#include <opencog/atoms/core/Quotation.h>

namespace opencog
{
class CondLink : public FunctionLink
{
protected:
	HandleSeq conds;
	HandleSeq exps;
	Handle default_exp;

	void init(void);

public:
	CondLink(const HandleSeq&&, Type=COND_LINK);
	CondLink(const CondLink&) = delete;
	CondLink& operator=(const CondLink&) = delete;

	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(CondLink)
#define createCondLink std::make_shared<CondLink>

/** @}*/
}

#endif // _OPENCOG_COND_LINK_H
