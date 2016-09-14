/**
 * Unify.h
 *
 * Utilities for unifying atoms.
 *
 * Copyright (C) 2016 OpenCog Foundation
 * All Rights Reserved
 * Author: Nil Geisweiller
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
 *
 * Created by Linas Vepstas February 2008
 */

#ifndef _OPENCOG_UNIFY_UTILS_H
#define _OPENCOG_UNIFY_UTILS_H

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/core/VariableList.h>

namespace opencog {

// Bool represents whether the mapping is valid
typedef std::pair<bool, HandleMap> BoolHandleMapPair;
typedef std::pair<bool, HandleMapSet> BoolHandleMapSetPair;
	
/**
 * Unify 2 atoms with variables on both sides and return a sequence of
 * mapping these variables to their corresponding matches.
 *
 * For instance (variable declarations left aside for simplicity):
 *
 * 1.
 *
 * unify((Variable "$X"), (Concept "A"))
 * ->
 * [{(Variable "$X"):(Concept "A")}]
 * 
 * 2.
 * 
 * unify((Concept "A"), (Concept "$X"))
 * ->
 * [{(Variable "$X"):(Concept "A")}]
 * 
 * 3.
 * 
 * unify((Inheritance (Concept "A") (Concept "B")), (Variable "$X"))
 * ->
 * [{(Variable "$X"):(Inheritance (Concept "A") (Concept "B"))}]
 * 
 * 4.
 * 
 * unify((Inheritance (Concept "A") (Variable "$Y")),
 *       (Inheritance (Variable "$X") (Concept "B"))
 * ->
 * [{(Variable "$X"):(Concept "A"),
 *   (Variable "$Y"):(Concept "B")}]
 *
 * I didn't give an example with multiple mappings but they would
 * occur if for instance one uses an unordered link, thus containing
 * multiple possible permutations.
 *
 * The boolean indicates whether the unification was successful.
 */
BoolHandleMapSetPair unify(const Handle& lhs, const Handle& rhs,
                           const Handle& lhs_vardecl = Handle::UNDEFINED,
                           const Handle& rhs_vardecl = Handle::UNDEFINED);

BoolHandleMapSetPair unify_rec(const Handle& lhs, const Handle& rhs,
                               const Handle& lhs_vardecl = Handle::UNDEFINED,
                               const Handle& rhs_vardecl = Handle::UNDEFINED);
	
/**
 * Merge 2 BoolHandleMapSetPair. If there are inconsistent the merge
 * early aborts and the resulting BoolHandleMapSetPair will contain
 * the mapping so far and false as first element of the pair.
 *
 * TODO: possibly optimize it by using one the arguments as output.
 */
BoolHandleMapSetPair mapset_merge(const BoolHandleMapSetPair& lhs,
                                  const BoolHandleMapSetPair& rhs);

/**
 * Merge a HandleMap into a HandleMapSet (according to map_merge below).
 *
 * TODO: possibly optimize it by using one the arguments as output.
 */
HandleMapSet mapset_merge(const HandleMapSet& lhs, const HandleMap& rhs);

/**
 * Merge 2 HandleMap. Perform the union of all mappings with disjoint
 * keys. Only insert common keys if they have the same mapped values.
 *
 * TODO: possibly optimize it by using one the arguments as output.
 */
HandleMap map_merge(const HandleMap& lhs, const HandleMap& rhs);

/**
 * Like map_merge above, but fill the valid flag is the 2 HandleMap
 * are either inconsisten, or one of them is invalid.
 *
 * TODO: possibly optimize it by using one the arguments as output.
 */
BoolHandleMapPair map_merge(const BoolHandleMapPair& lhs,
                            const BoolHandleMapPair& rhs);

/**
 * Given 2 atoms, with at least a variable, return a map from that
 * variable to the other atom, it can go 2 ways if the 2 atoms are
 * variables. If the types match then return true and the mapping,
 * other return false.
 */
BoolHandleMapPair var_merge(const Handle& lhs, const Handle& rhs,
                            const Handle& lhs_vardecl,
                            const Handle& rhs_vardecl);

/**
 * Like var_merge but only map var to val and not the other way
 * around.
 */
BoolHandleMapPair oneway_var_merge(const Handle& var, const Handle& val,
                                   const Handle& vardecl);

/**
 * Generate a VariableList of the free variables of a given atom h.
 */
VariableListPtr gen_varlist(const Handle& h);

/**
 * Given an atom h and its variable declaration vardecl, turn the
 * vardecl into a VariableList if not already, and if undefined,
 * generate a VariableList of the free variables of h.
 */
VariableListPtr gen_varlist(const Handle& h, const Handle& vardecl);

std::string oc_to_string(const BoolHandleMapSetPair& bhmsp);
	
} // namespace opencog

#endif // _OPENCOG_UNIFY_UTILS_H
