/*
 * opencog/atoms/grounded/GroundedPredicateNode.h
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

#ifndef _OPENCOG_GROUNDED_PREDICATE_NODE_H
#define _OPENCOG_GROUNDED_PREDICATE_NODE_H

#include <opencog/atoms/execution/GroundedProcedureNode.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class AtomSpace;
class Runner;

/// Execute scheme, python or other things.
class GroundedPredicateNode : public GroundedProcedureNode
{
	void init();
	Runner* _runner;

public:
	GroundedPredicateNode(Type, const std::string);
	GroundedPredicateNode(const std::string);
	GroundedPredicateNode(const GroundedPredicateNode&) = delete;
	GroundedPredicateNode& operator=(const GroundedPredicateNode&) = delete;
	virtual ~GroundedPredicateNode();

	virtual ValuePtr execute(AtomSpace*, const Handle&, bool silent=false);

	static Handle factory(const Handle&);
};

NODE_PTR_DECL(GroundedPredicateNode)
#define createGroundedPredicateNode CREATE_DECL(GroundedPredicateNode)

/** @}*/
}

#endif // _OPENCOG_GROUNDED_PREDICATE_NODE_H
