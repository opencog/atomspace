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

#include <opencog/atoms/reduct/NumericOutLink.h>

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
class RandomNumberLink : public NumericOutLink
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

typedef std::shared_ptr<RandomNumberLink> RandomNumberLinkPtr;
static inline RandomNumberLinkPtr RandomNumberLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<RandomNumberLink>(h); }
static inline RandomNumberLinkPtr RandomNumberLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<RandomNumberLink>(a); }

#define createRandomNumberLink std::make_shared<RandomNumberLink>

/** @}*/
}

#endif // _OPENCOG_RANDOM_NUMBER_LINK_H
