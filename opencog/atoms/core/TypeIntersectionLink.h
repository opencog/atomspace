/*
 * opencog/atoms/core/TypeIntersectionLink.h
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

#ifndef _OPENCOG_TYPE_INTERSECTION_H
#define _OPENCOG_TYPE_INTERSECTION_H

#include <opencog/atoms/core/TypeChoice.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The TypeIntersectionLink link is used to hold a type description; it is
/// an anonymous (un-named) type that is an intersectino of everything
/// it holds. It's primary use is for specifying typed glob intervals,
/// although it is also intended for computing the intersection of types.
///
/// This class computes the intersection of two types:
///   `(TypeIntersectionLink (TypeChoice stuff) (TypeChoice other-stuff))`
///
/// For type-union, use
///   `(TypeChoice (TypeChoice stuff) (TypeChoice other-stuff))`
///
class TypeIntersectionLink : public TypeChoice
{
protected:
	void init(bool);
	void analyze(Handle, bool&);
public:
	TypeIntersectionLink(const HandleSeq&&, Type=TYPE_INTERSECTION_LINK, bool=false);

	TypeIntersectionLink(const TypeIntersectionLink&) = delete;
	TypeIntersectionLink& operator=(const TypeIntersectionLink&) = delete;

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<TypeIntersectionLink> TypeIntersectionLinkPtr;
static inline TypeIntersectionLinkPtr TypeIntersectionLinkCast(const Handle& h)
	{ return std::dynamic_pointer_cast<TypeIntersectionLink>(h); }
static inline TypeIntersectionLinkPtr TypeIntersectionLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<TypeIntersectionLink>(a); }

#define createTypeIntersectionLink std::make_shared<TypeIntersectionLink>

/** @}*/
}

#endif // _OPENCOG_TYPE_INTERSECTION_H
