/*
 * ChainerUtils.h
 *
 * Copyright (C) 2014 OpenCog Foundation
 *
 * Author: William Ma <https://github.com/williampma>
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

#ifndef _OPENCOG_CHAINER_UTILS_H
#define _OPENCOG_CHAINER_UTILS_H

#include <opencog/atomspace/atom_types.h>
#include <opencog/atomspace/Atom.h>
#include <opencog/atomspace/Handle.h>
#include <opencog/atomspace/types.h>

namespace opencog
{
/** \addtogroup grp_chainer
 *  @{
 */

/**
 * Given an atom (a link or node), Return all its children nodes
 * (i.e. traversing the outgoings recursively)
 *
 * @param hinput - an atoms to be looked
 * @param types - a list of type nodes to look for. if vector
 *                is empty, all kinds of nodes are looked
 * @return - a set of nodes
 */
void get_outgoing_nodes(const Handle& hinput,
                        UnorderedHandleSet& retur,
                        const std::vector<Type>& types =
                                      std::vector<Type>());

/**
 * Makes a one to one similarity matching. If the atoms
 * are of type UnorderedLink, does one vs all similarity
 * matching and removes the matched from matching list
 * immediately.
 *
 * @param h1  A handle
 * @param h2  A handle
 * @param strict_type_match A flag telling how type matching should be
 * done.
 *
 * @return  A boolean true if similar and false otherwise.
 */
bool are_similar(const Handle& h1, const Handle& h2, bool strict_type_match);

/**
 * Returns neighboring atoms, following incoming links and
 * returning their outgoing sets.
 *
 * @param h Get neighbours for the atom this handle points to.
 * @param fanin Whether directional (ordered) links point to this
 *              node should beconsidered.
 * @param fanout Whether directional (ordered) links point from this
 *               node to another should be considered.
 * @param linkType Follow only these types of links.
 * @param subClasses Follow subtypes of linkType too.
 */
HandleSeq get_neighbors(const Handle&, bool fanin=true, bool fanout=true,
                        Type linkType=LINK, bool subClasses=true);

/**
 * Return all atoms connected to h up to a given distance. Both
 * incomings and outgoings are considered (unlike getNeighbors).
 *
 * @param h     the center atom
 * @param dist  the maximum distance, or none if negative
 * @return      an UnorderedHandleSet of neighbors
 *
 * XXX FIXME -- this function is curently not used anywhere. Perhaps
 * it should be deleted?
 */
UnorderedHandleSet get_distant_neighbors(const Handle& h, int dist = 1);

/** @}*/
}


#endif // _OPENCOG_CHAINER_UTILS_H
