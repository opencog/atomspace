/*
 * opencog/atoms/flow/StringOfLink.h
 *
 * Copyright (C) 2024 Linas Vepstas
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

#ifndef _OPENCOG_STRING_OF_LINK_H
#define _OPENCOG_STRING_OF_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The StringOfLink converts Nodes of one type to another, as well
/// as converting StringValues to Nodes, and vice-versa. As always
/// the arguments are executed, in order to obtain a current value.
///
class StringOfLink : public FunctionLink
{
private:
	void init(void);

public:
	StringOfLink(const HandleSeq&&, Type=STRING_OF_LINK);
	StringOfLink(const Handle&, const Handle&);

	StringOfLink(const StringOfLink&) = delete;
	StringOfLink& operator=(const StringOfLink&) = delete;

	// Return a pointer to the value at the specified key.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(StringOfLink)
#define createStringOfLink CREATE_DECL(StringOfLink)

/** @}*/
}

#endif // _OPENCOG_STRING_OF_LINK_H
