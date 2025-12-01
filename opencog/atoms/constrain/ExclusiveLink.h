/*
 * opencog/atoms/constrain/ExclusiveLink.h
 *
 * Copyright (C) 2024 BrainyBlaze Dynamics, LLC
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

#ifndef _OPENCOG_EXCLUSIVE_LINK_H
#define _OPENCOG_EXCLUSIVE_LINK_H

#include <opencog/atoms/execution/EvaluatableLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The ExclusiveLink represents a constraint that requires all of its
 * members to be grounded to distinct (non-equal) atoms. That is, no
 * two members of the ExclusiveLink may have the same grounding.
 *
 * This is used during pattern matching to enforce exclusivity
 * constraints, particularly useful for problems like Sudoku where
 * each cell in a row, column, or box must have a distinct value.
 */
class ExclusiveLink : public EvaluatableLink
{
public:
	ExclusiveLink(const HandleSeq&&, Type=EXCLUSIVE_LINK);
	ExclusiveLink(const ExclusiveLink&) = delete;
	ExclusiveLink& operator=(const ExclusiveLink&) = delete;
	virtual ~ExclusiveLink() {}

	virtual void setAtomSpace(AtomSpace*);
	virtual bool bevaluate(AtomSpace*, bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ExclusiveLink)
#define createExclusiveLink CREATE_DECL(ExclusiveLink)

/** @}*/
}

#endif // _OPENCOG_EXCLUSIVE_LINK_H
