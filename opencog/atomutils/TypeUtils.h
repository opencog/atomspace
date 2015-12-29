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

#include <opencog/atomspace/Handle.h>
#include <opencog/atomspace/types.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Type checker.  Returns true if `val` is of type `type_spec`.
 * More precisely, returns true if `val` will fit into the type
 * specification given by `type_spec`; that the value and the type
 * specification can be connected, e.g. for beta-reduction, or for
 * pattern matching (searching).
 */
bool value_is_type(const Handle& type_spec, const Handle& val);

/**
 * Type matcher. Returns true if `left` can mate with `right`.
 * Here, `left` can be a type definition, and `right` can be
 * another type defintion, or a value.  Mating is possible whenever
 * `left` is broader, less restricitve than `right`; equivalently
 * if `right` is narrower than 'left`.
 *
 * Mating types and values:
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

bool type_match(const Handle&, const Handle&);

/**
 * Same as above, but return the composition (beta-reduction) of the
 * match. If the types do NOT match, theundefined handle is returned.
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
Handle type_compose(const Handle&, const Handle&);

/** @}*/
}


#endif // _OPENCOG_TYPE_UTILS_H
