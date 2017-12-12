/*
 * opencog/atoms/core/ComposeLink.h
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

#ifndef _OPENCOG_COMPOSE_LINK_H
#define _OPENCOG_COMPOSE_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * High level function for function composition, implements
 * https://en.wikipedia.org/wiki/Function_composition. For more info
 * see https://wiki.opencog.org/w/ComposeLink.
 */
class ComposeLink : public FunctionLink {
protected:
	static void check_type(Type t);

public:
	// XXX Need to make this public, so that the factory can call it!
	ComposeLink(const HandleSeq oset, Type = COMPOSE_LINK);

	ComposeLink(const Link& l);
	virtual ~ComposeLink() {}

	virtual Handle execute(AtomSpace* = nullptr) const;
	static Handle factory(const Handle&);
};

typedef std::shared_ptr<ComposeLink> ComposeLinkPtr;
static inline ComposeLinkPtr ComposeLinkCast(const Handle& h)
{
	AtomPtr a(h); return std::dynamic_pointer_cast<ComposeLink>(a);
}
static inline ComposeLinkPtr ComposeLinkCast(AtomPtr a)
{
	return std::dynamic_pointer_cast<ComposeLink>(a);
}

#define createComposeLink std::make_shared<ComposeLink>

/** @}*/

}

#endif // _OPENCOG_COMPOSE_LINK_H
