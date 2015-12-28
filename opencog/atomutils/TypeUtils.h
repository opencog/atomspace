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
 * Type checker.  Returns true if `val` is of type `deep`.
 * More precisely, returns true if `val` will fit into the type
 * specification given by `deep`; that the value and the type
 * specification can be connected, e.g. for beta-reduction, or for
 * pattern matching (searching).
 */
bool value_is_type(Handle deep, const Handle& val);

/** @}*/
}


#endif // _OPENCOG_TYPE_UTILS_H
