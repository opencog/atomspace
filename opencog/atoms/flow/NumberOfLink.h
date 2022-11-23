/*
 * opencog/atoms/flow/NumberOfLink.h
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

#ifndef _OPENCOG_NUMBER_OF_LINK_H
#define _OPENCOG_NUMBER_OF_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The NumberOfLink converts FloatValues to NumberNodes.
/// It executes the Atom that it wraps, and stuffs the result
/// into a NumberNode.
///
class NumberOfLink : public FunctionLink
{
private:
	void init(void);

public:
	NumberOfLink(const HandleSeq&&, Type=NUMBER_OF_LINK);
	NumberOfLink(const Handle&);

	NumberOfLink(const NumberOfLink&) = delete;
	NumberOfLink& operator=(const NumberOfLink&) = delete;

	// Return a pointer to the value at the specified key.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(NumberOfLink)
#define createNumberOfLink CREATE_DECL(NumberOfLink)

/** @}*/
}

#endif // _OPENCOG_NUMBER_OF_LINK_H
