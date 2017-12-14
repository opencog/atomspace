/*
 * opencog/atoms/core/CompositionLink.h
 *
 * Copyright (C) 2017 Nil Geisweiller
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

#ifndef _OPENCOG_COMPOSITION_LINK_H
#define _OPENCOG_COMPOSITION_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * High level function for function composition, implements
 * https://en.wikipedia.org/wiki/Function_composition. For more info
 * see https://wiki.opencog.org/w/CompositionLink.
 */
class CompositionLink : public FunctionLink {
protected:
	void check() const;

	/**
	 * Given a sequence of scopes return the intersection of their
	 * variable declarations.
	 *
	 * TODO: for now is only deals with ProjectLinks and one of the
	 * variable declaration. If more than one variable declaration
	 * exists then just make sure they are equivalent, as opposed to
	 * calculate their intersection.
	 */
	Variables variables_intersection(const HandleSeq& scopes) const;

	/**
	 * Return projection index of a given ProjectLink.
	 */
	static unsigned projection_index(const Handle& projection);

public:
	// XXX Need to make this public, so that the factory can call it!
	CompositionLink(const HandleSeq oset, Type = COMPOSITION_LINK);

	CompositionLink(const Link& l);
	virtual ~CompositionLink() {}

	/**
	 * TODO: explain what it does
	 *
	 * TODO: do we really need to support the AtomSpace?
	 */
	virtual Handle execute(AtomSpace* = nullptr) const;

	/**
	 * Given a new variable declaration and a sequence of values to
	 * substitute the variables of this scope link, create a new scope
	 * that is the composition of this scope with the provided values.
	 */
	Handle compose(const Handle& nvardecl, const HandleSeq& values) const;
	Handle compose(const Variables& nvars, const HandleSeq& values) const;

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<CompositionLink> CompositionLinkPtr;
static inline CompositionLinkPtr CompositionLinkCast(const Handle& h)
{
	AtomPtr a(h); return std::dynamic_pointer_cast<CompositionLink>(a);
}
static inline CompositionLinkPtr CompositionLinkCast(AtomPtr a)
{
	return std::dynamic_pointer_cast<CompositionLink>(a);
}

#define createCompositionLink std::make_shared<CompositionLink>

/** @}*/

}

#endif // _OPENCOG_COMPOSITION_LINK_H
