/*
 * opencog/atoms/flow/SetValueLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
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

#ifndef _OPENCOG_SET_VALUE_LINK_H
#define _OPENCOG_SET_VALUE_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The SetValueLink sets a Value on the indicated atom (first
/// argument) at the indicated key (second argument). The Value
/// to be set is obtained by executing the third argument.
///
class SetValueLink : public FunctionLink
{
public:
	SetValueLink(const HandleSeq&&, Type=SET_VALUE_LINK);

	SetValueLink(const SetValueLink&) = delete;
	SetValueLink& operator=(const SetValueLink&) = delete;

	// Return a pointer to the value that was set.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(SetValueLink)
#define createSetValueLink std::make_shared<SetValueLink>

/** @}*/
}

#endif // _OPENCOG_SET_VALUE_LINK_H
