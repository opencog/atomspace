/*
 * opencog/atoms/constrain/IdenticalLink.h
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

#ifndef _OPENCOG_IDENTICAL_LINK_H
#define _OPENCOG_IDENTICAL_LINK_H

#include <opencog/atoms/execution/EvaluatableLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The IdenticalLink implements syntactic equality: all Atoms in the
 * outgoing set that are  closed (contain no free variables) must be
 * identically the same Atom.
 *
 * This is used during pattern matching to enforce identity constraints.
 */
class IdenticalLink : public EvaluatableLink
{
protected:
	bool is_identical(void) const;
	virtual void setAtomSpace(AtomSpace*);

public:
	IdenticalLink(const HandleSeq&&, Type=IDENTICAL_LINK);
	IdenticalLink(const IdenticalLink&) = delete;
	IdenticalLink& operator=(const IdenticalLink&) = delete;

	virtual bool bevaluate(AtomSpace*, bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(IdenticalLink)
#define createIdenticalLink CREATE_DECL(IdenticalLink)

/** @}*/
}

#endif // _OPENCOG_IDENTICAL_LINK_H
