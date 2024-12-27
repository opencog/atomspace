/*
 * opencog/atoms/flow/ConcatenateLink.h
 *
 * Copyright (C) 2015, 2022, 2024 Linas Vepstas
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

#ifndef _OPENCOG_CONCATENATE_LINK_H
#define _OPENCOG_CONCATENATE_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The ConcatenateLink collapses a list-of-lists down to just
/// one level. Here, the lists may be either LinkValues or
/// Links.
///
class ConcatenateLink : public FunctionLink
{
public:
	ConcatenateLink(const HandleSeq&&, Type = CONCATENATE_LINK);
	ConcatenateLink(const ConcatenateLink&) = delete;
	ConcatenateLink& operator=(const ConcatenateLink&) = delete;

	// Return a SetLink
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ConcatenateLink)
#define createConcatenateLink CREATE_DECL(ConcatenateLink)

/** @}*/
}

#endif // _OPENCOG_CONCATENATE_LINK_H
