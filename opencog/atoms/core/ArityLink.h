/*
 * opencog/atoms/core/ArityLink.h
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

#ifndef _OPENCOG_ARITY_LINK_H
#define _OPENCOG_ARITY_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The ArityLink returns a NumberNode holding the arity of the wrapped
/// atom; its zero for Nodes and empty Links.
///
/// For example,
///
///     ArityLink
///         SomeAtom
///         OtherAtom
///
/// will return
///
///     NumberNode 2
///
class ArityLink : public FunctionLink
{
public:
	ArityLink(const HandleSeq&&, Type = ARITY_LINK);
	ArityLink(const ArityLink&) = delete;
	ArityLink& operator=(const ArityLink&) = delete;

	// Return a pointer to the atom being specified.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ArityLink)
#define createArityLink CREATE_DECL(ArityLink)

/** @}*/
}

#endif // _OPENCOG_ARITY_LINK_H
