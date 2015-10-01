/*
 * opencog/atoms/core/AssignLink.h
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

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The InsertLink is used to create atoms at some defered time in the
/// future, while still being able to describe and talk about them at
/// the present time. The issue that it is solving is this: when an
/// atom is explicitly named, it also becomes a member of the AtomSpace;
/// that is, it is brought into existance by naming it.  The usual
/// mechanism for deferring the creation of an atom is to place a
/// VariableNode in it; theatom is then created later, when the Variable
/// is given a value. The InsertLink provides an alternative mechanism,
/// without requiring the use of VariableNodes.
///
/// The InsertLink is a kind of executable link; that it, upon
/// execution, it transforms into another link.  It should have the
/// following form:
///
///    InsertLink
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
/// The RemoveLink is like the InsertLink, except that it causes the
/// indicated form to be deleted (removed from the atomspace). If the
/// RemoveLink contains variables, and those variables are free, then
/// all matching paterns will be deleted from the AtomSpace.
///
/// The InsertLink vaguely resembles the ProLog predicate "assert()" with
/// respect to what it does to the atomspace database.  The RemoveLink
/// behaves like the ProLog "retract()" predicate.
///
/// The AssignLink combines the Insert and Remove into one.  Since the
/// format does not allow a close specification of the exact link to be
/// removed, it will remove all atoms fitting the general pattern,
/// i.e. those links having the indicated link type, the same first
/// atom (SomeAtom, above), and the same arity.  It will not remove
/// those links containing variables, as these are not concrete
/// (closed), but open.  It will do this before performing the Insert.
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
///     InsertLink
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

class InsertLink : public AssignLink
{
protected:

public:
	InsertLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	InsertLink(Link &l);

	// Return a pointer to the atom being specified.
	virtual Handle execute(AtomSpace* = NULL) const;
};

typedef std::shared_ptr<InsertLink> InsertLinkPtr;
static inline InsertLinkPtr InsertLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<InsertLink>(a); }
static inline InsertLinkPtr InsertLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<InsertLink>(a); }

// XXX temporary hack ...
#define createInsertLink std::make_shared<InsertLink>

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
