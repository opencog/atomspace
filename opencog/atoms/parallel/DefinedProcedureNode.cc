/*
 * opencog/atoms/parallel/DefinedProcedureNode.cc
 *
 * Copyright (C) 2009, 2013-2015, 2020, 2024, 2025 Linas Vepstas
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

#include <opencog/atoms/parallel/DefinedProcedureNode.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atomspace/AtomSpace.h>

using namespace opencog;

/// DefinedProcedureNode
/// Look up the definition, and execute it, when called.
/// This enables direct calls, such as
/// (cog-execute! (DefinedProcedure "foo"))

DefinedProcedureNode::DefinedProcedureNode(const std::string&& str)
    : Node(DEFINED_PROCEDURE_NODE, std::move(str))
{
}

DefinedProcedureNode::DefinedProcedureNode(Type t, const std::string&& str)
    : Node(t, std::move(str))
{
	if (not is_type(DEFINED_PROCEDURE_NODE))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a DefinedProcedureNode, got %s", tname.c_str());
	}
}

ValuePtr DefinedProcedureNode::execute(AtomSpace* as,
                                       bool silent)
{
	Handle defn(DefineLink::get_definition(get_handle()));
	if (nullptr == defn)
		throw RuntimeException(TRACE_INFO,
			"DefinedProcedureNode \"%s\" is not defined", get_name().c_str());

	if (not defn->is_executable()) return defn;
	return defn->execute();
}

DEFINE_NODE_FACTORY(DefinedProcedureNode, DEFINED_PROCEDURE_NODE)
