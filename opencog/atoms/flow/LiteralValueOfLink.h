/*
 * opencog/atoms/flow/LiteralValueOfLink.h
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

#ifndef _OPENCOG_LITERAL_VALUE_OF_LINK_H
#define _OPENCOG_LITERAL_VALUE_OF_LINK_H

#include <opencog/atoms/flow/ValueOfLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The LiteralValueOfLink returns the value on the indicated atom
/// (first argument) at the indicated key (second argument). The
/// literal value is returned; that value is NOT executed.
///
class LiteralValueOfLink : public ValueOfLink
{
private:
	void init(void);

public:
	LiteralValueOfLink(const HandleSeq&&, Type=LITERAL_VALUE_OF_LINK);

	LiteralValueOfLink(const LiteralValueOfLink&) = delete;
	LiteralValueOfLink& operator=(const LiteralValueOfLink&) = delete;

	// Return a pointer to the value at the specified key.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(LiteralValueOfLink)
#define createLiteralValueOfLink CREATE_DECL(LiteralValueOfLink)

/** @}*/
}

#endif // _OPENCOG_LITERAL_VALUE_OF_LINK_H
