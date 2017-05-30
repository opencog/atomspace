/*
 * opencog/atoms/core/RandomChoiceLink.h
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

#ifndef _OPENCOG_RANDOM_CHOICE_LINK_H
#define _OPENCOG_RANDOM_CHOICE_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The RandomChoiceLink randomly selects and returns one of its
/// N atoms, with a uniform probability distribution.
///
/// For example,
///
///     RandomChoiceLink
///         SomeAtom
///         OtherAtom
///
/// will return either SomeAtom or OtherAtom, with a 50-50 probability.
///
/// There are two different extensions that support a non-uniform
/// probability: one taking the name of a vector of numeric weights, and
/// another taking the name of a schema that provides numeric weights.
///
/// An example of the weighted form is:
///     
///     RandomChoiceLink
///         ListLink
///             NumberNode 0.4
///             NumberNode 0.6
///         ListLink
///             SomeAtom
///             OtherAtom
///
/// This will select SomeAtom 40% of the time, and OtherAtom 60% of the
/// time.
///
class RandomChoiceLink : public FunctionLink
{
public:
	RandomChoiceLink(const HandleSeq&, Type=RANDOM_CHOICE_LINK);
	RandomChoiceLink(const Link &l);

	// Return a pointer to the atom being specified.
	virtual Handle execute(AtomSpace* = NULL) const;

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<RandomChoiceLink> RandomChoiceLinkPtr;
static inline RandomChoiceLinkPtr RandomChoiceLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<RandomChoiceLink>(AtomCast(h)); }
static inline RandomChoiceLinkPtr RandomChoiceLinkCast(const AtomPtr& a)
	{ return std::dynamic_pointer_cast<RandomChoiceLink>(a); }

// XXX temporary hack ...
#define createRandomChoiceLink std::make_shared<RandomChoiceLink>

/** @}*/
}

#endif // _OPENCOG_RANDOM_CHOICE_LINK_H
