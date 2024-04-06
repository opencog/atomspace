/*
 * opencog/atoms/flow/ValueShimLink.h
 *
 * Copyright (C) 2018 Linas Vepstas
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

#ifndef _OPENCOG_VALUE_SHIM_LINK_H
#define _OPENCOG_VALUE_SHIM_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The ValueShimLink returns the value on the indicated atom (first
/// argument) at the indicated key (second argument).
///
class ValueShimLink : public Link
{
private:
	ValuePtr val;

public:
	ValueShimLink(Type=VALUE_SHIM_LINK);
	ValueShimLink(const HandleSeq&, Type=VALUE_SHIM_LINK);

	ValueShimLink(const ValueShimLink&) = delete;
	ValueShimLink& operator=(const ValueShimLink&) = delete;

	void set_value(const ValuePtr& v) { val = v; }
	virtual ValuePtr execute(AtomSpace*, bool) { return val; }

	virtual void setAtomSpace(AtomSpace *);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ValueShimLink)
#define createValueShimLink CREATE_DECL(ValueShimLink)

/** @}*/
}

#endif // _OPENCOG_VALUE_SHIM_LINK_H
