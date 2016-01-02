/*
 * AtomSpaceUtils.h
 *
 * Copyright (C) 2014 OpenCog Foundation
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

#ifndef _OPENCOG_ATOMSPACE_UTILS_H
#define _OPENCOG_ATOMSPACE_UTILS_H

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/types.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Add a new node to the AtomSpace. A random 16-character string
 * will be appended to the provided name.
 **/
Handle add_prefixed_node(AtomSpace&, Type, const std::string& prefix = "");

/**
 * Given a Handle remove it and all its descendants (outgoings of
 * outgoings, etc). Atoms with incomings will not be removed. In case
 * not all atoms have been removed it return false, true otherwise.
 *
 * @param as The AtomSpace to the remove the atoms from
 *
 * @param h  The handle to remove (and its descendants)
 *
 * @return true if all atoms (h and its descendants) have been
 * removed, false otherwise
 *
 * Example: remove_hypergraph(as, h) with
 * h = InheritanceLink
 *        ConceptNode "A"
 *        ConceptNode "B"
 *
 * will remove the InheritanceLink, ConceptNode "A" and ConceptNode "B".
 */
bool remove_hypergraph(AtomSpace& as, Handle h);

/** @}*/
}

#endif // _OPENCOG_ATOMSPACE_UTILS_H
