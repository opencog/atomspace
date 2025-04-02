/*
 * opencog/atoms/execution/GroundedProcedureNode.h
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

#ifndef _OPENCOG_GROUNDED_PROCEDURE_NODE_H
#define _OPENCOG_GROUNDED_PROCEDURE_NODE_H

#include <opencog/atoms/base/Node.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class AtomSpace;

/// Virtual base class for all grounded nodes.
class GroundedProcedureNode : public Node
{
public:
	GroundedProcedureNode(Type t, const std::string s)
		: Node(t, std::move(s)) {}
	GroundedProcedureNode(const GroundedProcedureNode&) = delete;
	GroundedProcedureNode& operator=(const GroundedProcedureNode&) = delete;
	virtual ~GroundedProcedureNode() {};

	virtual ValuePtr execute_args(AtomSpace*, const ValuePtr&, bool silent=false) = 0;
};

NODE_PTR_DECL(GroundedProcedureNode)

/** @}*/
}

#endif // _OPENCOG_GROUNDED_PROCEDURE_NODE_H
