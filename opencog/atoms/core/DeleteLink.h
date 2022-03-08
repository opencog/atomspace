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

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/FreeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The DeleteLink is used to delete any atom that does not contain a
/// VariableNode.  That is, if one attempts to insert a DeleteLink into
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
	void setAtomSpace(AtomSpace *);
public:
	DeleteLink(const HandleSeq&&, Type=DELETE_LINK);

	DeleteLink(const DeleteLink&) = delete;
	DeleteLink& operator=(const DeleteLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(DeleteLink)
#define createDeleteLink CREATE_DECL(DeleteLink)

/** @}*/
}

#endif // _OPENCOG_DELETE_LINK_H
