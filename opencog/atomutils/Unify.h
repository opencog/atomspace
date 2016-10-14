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

#include <boost/operators.hpp>

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/atom_types.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atomutils/TypeUtils.h>

// TODO: turn this into a class because there are way too many
// auxiliary functions

namespace opencog {

// Mapping from partition blocks to type
typedef std::map<OrderedHandleSet, Handle> UnificationPartition;
typedef UnificationPartition::value_type UnificationBlock;
typedef std::set<UnificationPartition> UnificationPartitions;

// TODO: the type of a typed block is currently a handle of the
// variable or ground it is exists, instead of an actual type.
struct UnificationSolutionSet :
		public boost::equality_comparable<UnificationSolutionSet>
{
	// Default ctor
	UnificationSolutionSet(bool s = true,
	                       const UnificationPartitions& p = UnificationPartitions());

	// Whether the unification satisfiable. Not that satisfiable is
	// different than empty. An empty solution set may still be
	// satisfiable, that would be the case of two candidates that
	// match but have no variables.
	bool satisfiable;

	// Set of typed partitions
	UnificationPartitions partitions;

	bool operator==(const UnificationSolutionSet& other) const {
		return satisfiable == other.satisfiable
			and partitions == other.partitions;
	}
};

// Subtitution values and their corresponding variable declaration
// (cause some values will be variables).
typedef std::map<HandleMap, Handle> TypedSubstitutions;
typedef TypedSubstitutions::value_type TypedSubstitution;

/**
 * Generate typed substitution rules, given a UnificationSolutionSet
 * and the term from which to select the variables as values in case
 * multiple choices are possible.
 *
 * TODO: support generating the associated type declartion.
 */
TypedSubstitutions typed_substitutions(const UnificationSolutionSet& sol,
                                       const Handle& pre);

/**
 * This algorithm perform unification by recursively
 *
 * 1. Generate all equality partitions
 *
 * 2. Decorate partition blocks with types
 *
 * 3. Check that each partition is satisfiable
 *
 * For now the types in 2. are represented by the substitutions, for
 * instance the typed block {{X, A}, A} means that X is A. Later one
 * we will replace that by deep types as to represents things like
 * {{X, Y}, ConceptNode} meaning that X and Y must be concept nodes is
 * order to be satisfiable, of course the deep will still need to
 * capture grounds such as {{X, A}, A}, it's not clear excatly how,
 * maybe Linas deep type implementation can already do that.
 *
 * To solve 3, for each partition block, it computes the type that
 * intersects all its elements and repeat till a fixed point is
 * reached. To do that efficiently we would need to build a dependency
 * DAG, but at first we can afford to compute type intersections is
 * random order.
 *
 * Also, permutations are supported though very slow.
 *
 * Examples:
 *
 * 1.
 *
 * unify((Variable "$X"), (Concept "A"))
 * ->
 * {{<{(Variable "$X"), (Concept "A")}, (Concept "A")>}}
 *
 * meaning that the partition block {(Variable "$X"), (Concept "A")}
 * has type (Concept "A"), and there is only one partition in the
 * solution set.
 *
 * 2.
 *
 * unify((Concept "A"), (Concept "$X"))
 * ->
 * {{<{(Variable "$X"), (Concept "A")}, (Concept "A")>}}
 *
 * 3.
 *
 * unify((Inheritance (Concept "A") (Concept "B")), (Variable "$X"))
 * ->
 * {{<{(Variable "$X"), (Inheritance (Concept "A") (Concept "B"))},
 *    (Inheritance (Concept "A") (Concept "B"))>}}
 *
 * 4.
 *
 * unify((Inheritance (Concept "A") (Variable "$Y")),
 *       (Inheritance (Variable "$X") (Concept "B"))
 * ->
 * {{<{(Variable "$X"), (Concept "A")}, (Concept "A")>,
 *   <{(Variable "$Y"), (Concept "B")}, (Concept "B")>}}
 *
 * 5.
 *
 * unify((And (Concept "A") (Concept "B")),
 *       (And (Variable "$X") (Variable "$Y"))
 * ->
 * {
 *  {<{(Variable "$X"), (Concept "A")}, (Concept "A")>,
 *   <{(Variable "$Y"), (Concept "B")}, (Concept "B")>},
 *
 *  {<{(Variable "$X"), (Concept "B")}, (Concept "B")>,
 *   <{(Variable "$Y"), (Concept "A")}, (Concept "A")>}
 * }
 *
 * mean that the solution set has 2 partitions, one where X unifies to
 * A and Y unifies to B, and another one where X unifies to B and Y
 * unifies to A.
 *
 * TODO: take care of Un/Quote and Scope links.
 */
UnificationSolutionSet unify(const Handle& lhs, const Handle& rhs,
                             const Handle& lhs_vardecl = Handle::UNDEFINED,
                             const Handle& rhs_vardecl = Handle::UNDEFINED);
UnificationSolutionSet unordered_unify(const HandleSeq& lhs,
                                       const HandleSeq& rhs,
                                       const Handle& lhs_vardecl,
                                       const Handle& rhs_vardecl);
UnificationSolutionSet ordered_unify(const HandleSeq& lhs,
                                     const HandleSeq& rhs,
                                     const Handle& lhs_vardecl,
                                     const Handle& rhs_vardecl);

/**
 * Return if the atom is an unordered link.
 */
bool is_unordered(const Handle& h);

/**
 * Return a copy of a HandleSeq with the ith element removed.
 */
HandleSeq cp_erase(const HandleSeq& hs, Arity i);

/**
 * Build elementary solution set between 2 atoms given that at least
 * one of them is a variable.
 */
UnificationSolutionSet mkvarsol(const Handle& lhs, const Handle& rhs,
                                const Handle& lhs_vardecl,
                                const Handle& rhs_vardecl);

/**
 * Join 2 solution sets. Generate the product of all consistent
 * solutions (with partitions so that all blocks are typed with a
 * defined Handle).
 */
UnificationSolutionSet join(const UnificationSolutionSet& lhs,
                             const UnificationSolutionSet& rhs);

// TODO: add comment. Possibly return UnificationPartitions instead of
// UnificationSolutionSet.
UnificationPartitions join(const UnificationPartitions& lhs,
                            const UnificationPartition& rhs);

/**
 * Join 2 partitions. If the resulting partition is satisfiable then
 * the empty partition is returned.
 */
UnificationPartition join(const UnificationPartition& lhs,
                           const UnificationPartition& rhs);

/**
 * Join 2 blocks (supposedly satisfiable).
 *
 * That is compute their type intersection and if defined, then build
 * the block as the union of the 2 blocks, typed with their type
 * intersection.
 */
UnificationBlock join(const UnificationBlock& lhs, const UnificationBlock& rhs);

/**
 * Return true if a unification block is satisfiable. A unification
 * block is non satisfiable if it's type is undefined (bottom).
 */
bool is_satisfiable(const UnificationBlock& block);

/**
 * Calculate type intersection. For example: say you have for a block
 * with
 *
 * X
 * ListLink(Y)
 * ListLink(Z)
 *
 * meaning that X is equal to ListLink Y which is equal to ListLink Z,
 * each having the following types at that point (i.e. not having
 * reached the fixed point yet)
 *
 * X:Atom
 * ListLink(Y):ListLink(Atom)
 * ListLink(Z):ListLink(Atom)
 *
 * then their type intersection will be
 *
 * ListLink(Atom)
 *
 * which is supposed to represent the set of all potential groundings
 * that may satisfy that block.
 *
 * TODO: this can be probably by optimized by using VariableListPtr
 *       instead of Handle, so we don't rebuild it every time.
 */
Handle type_intersection(const Handle& lhs, const Handle& rhs,
                         const Handle& lhs_vardecl = Handle::UNDEFINED,
                         const Handle& rhs_vardecl = Handle::UNDEFINED);

/**
 * Return a simplification of a type union, by eliminating all types
 * that are redundant. For instance
 *
 * {Node, ConceptNode, ListLink}
 *
 * would return
 *
 * {Node, ListLink}
 *
 * As ConceptNode inherits Node.
 */
std::set<Type> simplify_type_union(std::set<Type>& type);

/**
 * Return the union type of a variable given its variable declaration.
 * If the variable declaration is empty (Handle::UNDEFINED) then the
 * union type is not empty, instead it contains the singleton
 * {ATOM}. An empty union type would instead mean the bottom type
 * (that nothing can inherit).
 */
std::set<Type> get_union_type(const Handle& h, const Handle& vardecl);

/**
 * Return true if lhs inherit rhs. If lhs is not a variable then it
 * relays that to VariableList::is_type, otherwise their type
 * declarations are compared.
 */
bool inherit(const Handle& lhs, const Handle& rhs,
             const Handle& lhs_vardecl, const Handle& rhs_vardecl);

/**
 * Extreme crude version of the above when we have no variable
 * declarations. Basically 2 variables inherits each other and a non
 * variable inherits a variable. Everything else return false.
 */
bool inherit(const Handle& lhs, const Handle& rhs);

/**
 * Return true if lhs inherits rhs.
 */
bool inherit(Type lhs, Type rhs);

/**
 * Return true if a type inherits a type union.
 */
bool inherit(Type lhs, const std::set<Type>& rhs);

/**
 * Return true if lhs inherits rhs. That is if all elements of lhs
 * inherits rhs.
 */
bool inherit(const std::set<Type>& lhs, const std::set<Type>& rhs);

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

/**
 * Merge two vardecls into one. If a variable is present in both
 * vardecls then the more restrictive one replaces the less
 * restrictive one.
 *
 * TODO: give example.
 *
 * TODO: this might be moved to TypeUtils.{h,cc}
 *
 * This might correspond to type product, I'm not sure.
 *
 * TODO: use Variables::extend
 */
Handle merge_vardecl(const Handle& lhs_vardecl, const Handle& rhs_vardecl);

/**
 * TODO
 */
Handle find_variable(const Handle& vardecl, const Handle& variable);

std::string oc_to_string(const UnificationPartition& hshm);
std::string oc_to_string(const UnificationBlock& ub);
std::string oc_to_string(const UnificationPartitions& par);
std::string oc_to_string(const UnificationSolutionSet& sol);
std::string oc_to_string(const TypedSubstitutions& tss);
std::string oc_to_string(const TypedSubstitution& ts);
	
} // namespace opencog

#endif // _OPENCOG_UNIFY_UTILS_H
