/*
 * opencog/atoms/constrain/EqualLink.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, LLC
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

#ifndef _OPENCOG_EQUAL_LINK_H
#define _OPENCOG_EQUAL_LINK_H

#include <opencog/atoms/execution/EvaluatableLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The EqualLink implements semantic equality: the contents are equal
 * if, after execution, they result in the same Atom or Value.
 *
 * This is used during pattern matching to enforce equality constraints.
 */
class EqualLink : public EvaluatableLink
{
protected:
	virtual void setAtomSpace(AtomSpace*);
public:
	EqualLink(const HandleSeq&&, Type=EQUAL_LINK);
	EqualLink(const EqualLink&) = delete;
	EqualLink& operator=(const EqualLink&) = delete;

	virtual bool bevaluate(AtomSpace*, bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(EqualLink)
#define createEqualLink CREATE_DECL(EqualLink)

/** @}*/
}

#endif // _OPENCOG_EQUAL_LINK_H
