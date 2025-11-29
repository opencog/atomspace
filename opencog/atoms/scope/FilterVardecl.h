/*
 * opencog/atoms/scope/FilterVardecl.h
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

#ifndef _OPENCOG_FILTER_VARDECL_H
#define _OPENCOG_FILTER_VARDECL_H

#include <opencog/atoms/base/Handle.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

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

/** @}*/
}

#endif // _OPENCOG_FILTER_VARDECL_H
