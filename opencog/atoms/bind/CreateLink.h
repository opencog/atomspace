/*
 * opencog/atoms/CreateLink.h
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

#ifndef _OPENCOG_CREATE_LINK_H
#define _OPENCOG_CREATE_LINK_H

#include <map>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Link.h>
#include <opencog/atoms/bind/ScopeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The CreateLink is used to create atoms at some defered time in the
/// future, while still being able to describe and talk about them at
/// the present time. The issue that it is solving is this: when an
/// atom is explicitly named, it also becomes a member of the AtomSpace;
/// that is, it is brought into existance by naming it.  The usual
/// mechanism for deferring the creation of an atom is to place a
/// VariableNode in it; theatom is then created later, when the Variable
/// is given a value. The CreateLink provides an alternative mechanism,
/// without requiring the use of VariableNodes.
///
/// The CreateLink is a kind of executable link; that it, upon
/// execution, it transforms into another link.  It should have the
/// following form:
///
///    CreateLink
///         TypeNode "SomeLink"
///         SomeAtom
///         OtherAtom
///
/// Upon execution, this will result in the following being created:
///
///    SomeLink
///         SomeAtom
///         OtherAtom
///
class CreateLink : public Link
{
protected:
	void init(const HandleSeq&);
	Type _link_type;
	HandleSeq _outset;

public:
	CreateLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	CreateLink(const Handle& varcdecls, const Handle& body,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	CreateLink(Link &l);

	// Return a poointer to the atom being specified.
	LinkPtr create(void) const;
};

typedef std::shared_ptr<CreateLink> CreateLinkPtr;
static inline CreateLinkPtr CreateLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<CreateLink>(a); }
static inline CreateLinkPtr CreateLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<CreateLink>(a); }

// XXX temporary hack ...
#define createCreateLink std::make_shared<CreateLink>

/** @}*/
}

#endif // _OPENCOG_CREATE_LINK_H
