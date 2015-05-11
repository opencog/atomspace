/*
 * opencog/atoms/AssignLink.h
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

#ifndef _OPENCOG_ASSIGN_LINK_H
#define _OPENCOG_ASSIGN_LINK_H

#include <map>

#include <opencog/atoms/reduct/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The AddLink is used to create atoms at some defered time in the
/// future, while still being able to describe and talk about them at
/// the present time. The issue that it is solving is this: when an
/// atom is explicitly named, it also becomes a member of the AtomSpace;
/// that is, it is brought into existance by naming it.  The usual
/// mechanism for deferring the creation of an atom is to place a
/// VariableNode in it; theatom is then created later, when the Variable
/// is given a value. The AddLink provides an alternative mechanism,
/// without requiring the use of VariableNodes.
///
/// The AddLink is a kind of executable link; that it, upon
/// execution, it transforms into another link.  It should have the
/// following form:
///
///    AddLink
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
/// The RemoveLink is like the AddLink, except that it causes the
/// indicated form to be deleted (removed from the atomspace). If the
/// RemoveLink contains variables, and those variables are free, then
/// all matching paterns will be deleted from the AtomSpace.
///
/// The AddLink vaguely resembles the ProLog predicate "assert()" with
/// respect to what it does to the atomspace database.  The RemoveLink
/// behaves like the ProLog "retract()" predicate.
///
/// The AssignLink combines the Add and Remove into one.  Since the
/// format does not allow a close specification of the exact link to be
/// removed, it will remove all atoms fitting the general pattern,
/// i.e. those links having the indicated link type, the same first
/// atom (SomeAtom, above), and the same arity.  It will not remove
/// those links containing variables, as these are not concrete
/// (closed), but open.  It will do this before performing the Add.
///
/// To be precise:
///
///     AssignLink
///         TypeNode "SomeLink"
///         SomeAtom
///         OtherAtom
///
/// is identical to saying:
///
///     RemoveLink
///         TypeNode "SomeLink"
///         SomeAtom
///         VariableNode "$free"
///
///     AddLink
///         TypeNode "SomeLink"
///         SomeAtom
///         OtherAtom
///
class AssignLink : public FunctionLink
{
protected:
	void init(const HandleSeq&);
	Type _link_type;
	HandleSeq _outset;
	size_t _osetz;

public:
	AssignLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	AssignLink(Type t, const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	AssignLink(Link &l);

	// Return a pointer to the atom being specified.
	virtual Handle execute(AtomSpace* = NULL) const;
};

typedef std::shared_ptr<AssignLink> AssignLinkPtr;
static inline AssignLinkPtr AssignLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<AssignLink>(a); }
static inline AssignLinkPtr AssignLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<AssignLink>(a); }

// XXX temporary hack ...
#define createAssignLink std::make_shared<AssignLink>

class AddLink : public AssignLink
{
protected:

public:
	AddLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	AddLink(Link &l);

	// Return a pointer to the atom being specified.
	virtual Handle execute(AtomSpace* = NULL) const;
};

typedef std::shared_ptr<AddLink> AddLinkPtr;
static inline AddLinkPtr AddLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<AddLink>(a); }
static inline AddLinkPtr AddLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<AddLink>(a); }

// XXX temporary hack ...
#define createAddLink std::make_shared<AddLink>

class RemoveLink : public AssignLink
{
protected:

public:
	RemoveLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	RemoveLink(Link &l);

	// Return a pointer to the atom being specified.
	virtual Handle execute(AtomSpace* = NULL) const;
};

typedef std::shared_ptr<RemoveLink> RemoveLinkPtr;
static inline RemoveLinkPtr RemoveLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<RemoveLink>(a); }
static inline RemoveLinkPtr RemoveLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<RemoveLink>(a); }

// XXX temporary hack ...
#define createRemoveLink std::make_shared<RemoveLink>

/** @}*/
}

#endif // _OPENCOG_ASSIGN_LINK_H
