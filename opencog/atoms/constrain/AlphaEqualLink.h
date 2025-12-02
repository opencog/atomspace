/*
 * opencog/atoms/constrain/AlphaEqualLink.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics LLC
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

#ifndef _OPENCOG_ALPHA_EQUAL_LINK_H
#define _OPENCOG_ALPHA_EQUAL_LINK_H

#include <opencog/atoms/execution/EvaluatableLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The AlphaEqualLink checks for alpha-equivalence between two atoms.
 * Two atoms are alpha-equivalent if they are structurally identical
 * after variable renaming (alpha-conversion).
 *
 * For example, (Lambda (Variable "x") (Concept "foo"))
 * is alpha-equivalent to (Lambda (Variable "y") (Concept "foo"))
 * because the bound variables can be renamed without changing meaning.
 */
class AlphaEqualLink : public EvaluatableLink
{
public:
	AlphaEqualLink(const HandleSeq&&, Type=ALPHA_EQUAL_LINK);
	AlphaEqualLink(const AlphaEqualLink&) = delete;
	AlphaEqualLink& operator=(const AlphaEqualLink&) = delete;
	virtual ~AlphaEqualLink() {}

	virtual bool bevaluate(AtomSpace*, bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(AlphaEqualLink)
#define createAlphaEqualLink CREATE_DECL(AlphaEqualLink)

/** @}*/
}

#endif // _OPENCOG_ALPHA_EQUAL_LINK_H
