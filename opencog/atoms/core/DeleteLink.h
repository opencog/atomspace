/*
 * opencog/atoms/core/DeleteLink.h
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

#ifndef _OPENCOG_DELETE_LINK_H
#define _OPENCOG_DELETE_LINK_H

#include <map>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/Link.h>
#include <opencog/atoms/core/FreeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The DeleteLink is used to delete any atom that is not a
/// VariableNode.  That is, if in attempts to insert a DeleteLink into
/// the atomspace, and the DeleteLink does not have any VariableNodes
/// in it, the insertion will fail, and furthermore, the atom(s)
/// that it is holding (in its outgoing set) will be deleted from the
/// atomspace!  In essence, the DeleteLink is a link that can never be
/// grounded!
///
class DeleteLink : public FreeLink
{
protected:
	void init(void);
public:
	DeleteLink(const HandleSeq&,
	           TruthValuePtr tv = TruthValue::DEFAULT_TV(),
	           AttentionValuePtr av = AttentionValue::DEFAULT_AV());

	DeleteLink(Link &l);
};

typedef std::shared_ptr<DeleteLink> DeleteLinkPtr;
static inline DeleteLinkPtr DeleteLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<DeleteLink>(a); }
static inline DeleteLinkPtr DeleteLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<DeleteLink>(a); }

// XXX temporary hack ...
#define createDeleteLink std::make_shared<DeleteLink>

/** @}*/
}

#endif // _OPENCOG_DELETE_LINK_H
