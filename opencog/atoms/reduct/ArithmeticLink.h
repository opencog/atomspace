/*
 * opencog/atoms/reduct/ArithmeticLink.h
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
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

#ifndef _OPENCOG_ARITHMETIC_LINK_H
#define _OPENCOG_ARITHMETIC_LINK_H

#include <opencog/atoms/reduct/FoldLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The ArithmeticLink implements the simple arithmetic operations.
 * It uses FoldLink to perform delta-reduction.
 */
class ArithmeticLink : public FoldLink
{
protected:
	void init(void);

	virtual Handle reorder(void) const;
	bool _commutative;


public:
	ArithmeticLink(const HandleSeq&&, Type=ARITHMETIC_LINK);

	ArithmeticLink(const ArithmeticLink&) = delete;
	ArithmeticLink& operator=(const ArithmeticLink&) = delete;

	virtual ValuePtr delta_reduce(AtomSpace*, bool) const;
	virtual ValuePtr execute(AtomSpace*, bool);
	virtual ValuePtr execute(void) { return execute(_atom_space, false); }
};

LINK_PTR_DECL(ArithmeticLink)
#define createArithmeticLink CREATE_DECL(ArithmeticLink)

/** @}*/
}

#endif // _OPENCOG_ARITHMETIC_LINK_H
