/*
 * opencog/atoms/reduct/RandomNumber.h
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

#ifndef _OPENCOG_RANDOM_NUMBER_LINK_H
#define _OPENCOG_RANDOM_NUMBER_LINK_H

#include <opencog/atoms/reduct/NumericFunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The RandomNumberLink returns a NumberNode that lies within the
/// min-max range, using a uniform distribution.
///
/// For example,
///
///     RandomNumberLink
///         NumberNode 0.1
///         NumberNode 0.5
///
/// will return a random number between 0.1 ad 0.5
///
class RandomNumberLink : public NumericFunctionLink
{
protected:
	void init();

public:
	RandomNumberLink(const HandleSeq&&, Type=RANDOM_NUMBER_LINK);
	RandomNumberLink(const RandomNumberLink&) = delete;
	RandomNumberLink& operator=(const RandomNumberLink&) = delete;

	// Return a pointer to the atom being specified.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(RandomNumberLink)
#define createRandomNumberLink CREATE_DECL(RandomNumberLink)

/** @}*/
}

#endif // _OPENCOG_RANDOM_NUMBER_LINK_H
