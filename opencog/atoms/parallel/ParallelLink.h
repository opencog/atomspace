/*
 * opencog/atoms/parallel/ParallelLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
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

#ifndef _OPENCOG_PARALLEL_LINK_H
#define _OPENCOG_PARALLEL_LINK_H

#include <opencog/atoms/core/UnorderedLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class AtomSpace;

class ParallelLink : public UnorderedLink
{
public:
	ParallelLink(const HandleSeq&&, Type=PARALLEL_LINK);
	ParallelLink(const ParallelLink&) = delete;
	ParallelLink& operator=(const ParallelLink&) = delete;

	virtual bool is_evaluatable() const { return true; }
	virtual bool bevaluate(AtomSpace*, bool);
	void evaluate_scratch(AtomSpace*, bool, AtomSpace*);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ParallelLink)
#define createParallelLink CREATE_DECL(ParallelLink)

/** @}*/
}

#endif // _OPENCOG_PARALLEL_LINK_H
