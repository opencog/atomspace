/*
 * opencog/atoms/constrain/ExclusiveLink.h
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

#ifndef _OPENCOG_EXCLUSIVE_LINK_H
#define _OPENCOG_EXCLUSIVE_LINK_H

#include <opencog/atoms/execution/EvaluatableLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The ExclusiveLink implements exclusive-choice. All Atoms in the
 * oset MUST differ from one-another; if they do not, then this
 * Link cannot be added to the AtomSpace.
 *
 * This Link is Evaluatable: if any Atoms in it's outgoing set are
 * executable, and that execution returns an Atom, then all such
 * results must be exclusively unique, else false is returned.
 */
class ExclusiveLink : public EvaluatableLink
{
protected:
	virtual void setAtomSpace(AtomSpace*);

public:
	ExclusiveLink(const HandleSeq&&, Type=EXCLUSIVE_LINK);
	ExclusiveLink(const ExclusiveLink&) = delete;
	ExclusiveLink& operator=(const ExclusiveLink&) = delete;

	virtual bool bevaluate(AtomSpace*, bool silent=false);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ExclusiveLink)
#define createExclusiveLink CREATE_DECL(ExclusiveLink)

/** @}*/
}

#endif // _OPENCOG_EXCLUSIVE_LINK_H
