/*
 * TypeUtils.h
 *
 * Copyright (C) 2015 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com> December 2015
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

#ifndef _OPENCOG_TYPE_UTILS_H
#define _OPENCOG_TYPE_UTILS_H

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/core/VariableSet.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Type checker.  Returns true if `value` is of type `type_spec`.
 * More precisely, returns true if `value` will fit into the type
 * specification given by `type_spec`; that the value and the type
 * specification can be connected, e.g. for beta-reduction, or for
 * pattern matching (searching).
 */
bool value_is_type(const Handle& type_spec, const ValuePtr& value);

/**
 * Type matcher. Returns true if `left` can mate with `right`.
 * Here, `left` can be a type definition, and `right` can be
 * another type definition, or a value.  Mating is possible whenever
 * `left` is broader, less restricitve than `right`; equivalently
 * if `right` is narrower than 'left`.
 *
 * Mating types and arguments:
 * left == (Type "Concept")    right == (Concept "foo")  can mate.
 * left == (Type "Concept")    right == (Number 13)  cannot.
 *
 * Mating types and types:
 * left == (Type "Concept")    right == (Type "Concept")  can mate.
 *
 * Left is wider (polymorphic, in this case)
 *   left == (TypeChoice (Type "Concept") (Type "Number"))
 *   right == (Type "Number")  can mate.
 *
 * Function call arguments can be checked:
 *   left == (Arrow (Type "Concept") (Type "Number"))
 *   right == (Concept "foo")  can mate.
 *
 *   left == (Arrow (Type "Concept") (Type "Number"))
 *   right == (Number 13)  cannot.
 *
 * Function call chains can be checked:
 *   left == (Arrow (Type "Concept") (Type "Number"))
 *   right == (Type "Concept")  can mate.
 *
 * The following can mate, because left accepts a concept as input,
 * and right generates a concept as output:
 *   left == (Arrow (Type "Concept") (Type "Number"))
 *   right == (Arrow (Type "Evaluation") (Type "Concept")
 *
 * Any type specification is valid: SignatureLinks, etc work too.
 */
bool type_match(const Handle&, const ValuePtr&);

/**
 * Same as above, but return the composition (beta-reduction) of the
 * match. If the types do NOT match, an exception is thrown.
 * If the types do match, then, for many cases, the right side is the
 * result.  The compostion of arrows, however, results either in a
 * new arrow, or a simple return type.
 *
 * Examples:
 *
 * Function call arguments can be checked:
 *   left == (Arrow (Type "Concept") (Type "Number"))
 *   right == (Concept "foo")  can mate.
 *   result = (Type "Number")
 *
 * Function call chains:
 *   left == (Arrow (Type "Concept") (Type "Number"))
 *   right == (Type "Concept")  can mate.
 *   result = (Type "Number")
 *
 * The following can mate, because left accepts a concept as input,
 * and right generates a concept as output:
 *   left == (Arrow (Type "Concept") (Type "Number"))
 *   right == (Arrow (Type "Evaluation") (Type "Concept")
 *   result = (Arrow (Type "Evaluation") (Type "Number"))
 */
ValuePtr type_compose(const Handle&, const ValuePtr&);

/**
 * Given a variable declaration (`VariableList`) and a pattern body,
 * remove all variables in the declaration that are not present in
 * the pattern body, and return the smaller (minimal) `VariableList`.
 * (That is, return the list of only those variables that appear in
 * the body).
 *
 * For instance, `filter_vardecl()` applied to
 *
 * var_decl
 * =
 * (VariableList
 *    (TypedVariableLink
 *       (VariableNode "$X")
 *       (TypeNode "ConceptNode"))
 *    (TypedVariableLink
 *       (VariableNode "$Y")
 *       (TypeNode "ConceptNode")))
 *
 * body
 * =
 * (InheritanceLink
 *    (ConceptNode "human")
 *    (VariableNode "$Y"))
 *
 * will return
 *
 * (TypedVariableLink
 *    (VariableNode "$Y")
 *    (TypeNode "ConceptNode"))
 *
 * Special cases:
 *
 * 1. The `VariableList` is discarded if the resulting variable
 *    declaration contains only one variable.
 *
 * 2. If nothing is left after filtering it returns Handle::UNDEFINED
 *
 * 3. If vardecl is Handle::UNDEFINED, then return Handle::UNDEFINED
 *
 * The resulting variable declaration will not be added to any
 * AtomSpace, it's up to the user to do that.
 *
 * FYI, the Variables::trim() method does almost the same thing, except
 * that it cuts down the vardecl "in-place", instead of bulding a new
 * one, like this does.
 */
Handle filter_vardecl(const Handle& vardecl, const Handle& body);

/**
 * Like filter_vardecl(const Handle& vardecl, const Handle& body)
 * except that the variable needs to be in at least one body of hs.
 */
Handle filter_vardecl(const Handle& vardecl, const HandleSeq& hs);

/**
 * Return true if t is different than NOTYPE.
 */
bool is_well_typed(Type t);

/**
 * Return true if all types in ts are well typed.
 *
 * This might too strict. One might argue that NOTYPE is akin to the
 * empty set, thus the union of a valid type and NOTYPE should merely
 * amount to the valid type. We may want to relax that definition in
 * the future.
 */
bool is_well_typed(const TypeSet& ts);

/** @}*/
}

#endif // _OPENCOG_TYPE_UTILS_H
