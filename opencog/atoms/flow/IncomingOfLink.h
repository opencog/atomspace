/*
 * opencog/atoms/flow/IncomingOfLink.h
 *
 * Copyright (C) 2015, 2022 Linas Vepstas
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

#ifndef _OPENCOG_INCOMING_OF_LINK_H
#define _OPENCOG_INCOMING_OF_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The IncomingOfLink returns a LinkValue of the IncomingSet of
/// the wrapped atom. Optionally, a simple type can be specified.
///
/// For example,
///
///     IncomingOfLink
///         Predicate "foo"
///         TypeNode 'EvaluationLink
///
/// will return
///
///     (LinkValue (Evaluation (Predicate "foo") ...) ...)
///
class IncomingOfLink : public FunctionLink
{
public:
	IncomingOfLink(const HandleSeq&&, Type = INCOMING_OF_LINK);
	IncomingOfLink(const IncomingOfLink&) = delete;
	IncomingOfLink& operator=(const IncomingOfLink&) = delete;

	// Return a pointer to LinkValue holding the IncomingSet.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(IncomingOfLink)
#define createIncomingOfLink CREATE_DECL(IncomingOfLink)

/** @}*/
}

#endif // _OPENCOG_INCOMING_OF_LINK_H
