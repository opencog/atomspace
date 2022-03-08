/*
 * opencog/atoms/core/ImplicationScopeLink.h
 *
 * Copyright (C) 2015 OpenCog Foundation
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
 */

#ifndef _OPENCOG_IMPLICATION_SCOPE_LINK_H
#define _OPENCOG_IMPLICATION_SCOPE_LINK_H

#include <opencog/atoms/core/ScopeLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// ImplicationScopeLink
///    <variables>
///    <P-body>
///    <Q-body>
///
/// Is a syntactic sugar form for
///
/// ImplicationLink
///    LambdaLink
///       <variables>
///       <P-body>
///    LambdaLink
///       <variables>
///       <Q-body>
class ImplicationScopeLink : public ScopeLink
{
protected:
	void init(void);
	void extract_variables(const HandleSeq& oset);
	// Useless for now, but...
	Handle _implicand;
public:
	ImplicationScopeLink(const HandleSeq&&, Type = IMPLICATION_SCOPE_LINK);
	ImplicationScopeLink(const ImplicationScopeLink &) = delete;
	ImplicationScopeLink& operator=(const ImplicationScopeLink &) = delete;

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ImplicationScopeLink)
#define createImplicationScopeLink std::make_shared<ImplicationScopeLink>

/** @}*/
}

#endif // _OPENCOG_IMPLICATION_SCOPE_LINK_H
