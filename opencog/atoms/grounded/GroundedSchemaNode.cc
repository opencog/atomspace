/*
 * opencog/atoms/grounded/GroundedSchemaNode.cc
 *
 * Copyright (C) 2009, 2013, 2015, 2020 Linas Vepstas
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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atomspace/AtomSpace.h>

#include <opencog/atoms/grounded/GroundedSchemaNode.h>
#include "LibraryRunner.h"
#include "PythonRunner.h"
#include "SCMRunner.h"

using namespace opencog;

GroundedSchemaNode::GroundedSchemaNode(std::string s)
	: GroundedProcedureNode(GROUNDED_SCHEMA_NODE, std::move(s))
{
	init();
}

GroundedSchemaNode::GroundedSchemaNode(Type t, std::string s)
	: GroundedProcedureNode(t, std::move(s))
{
	if (not nameserver().isA(t, GROUNDED_SCHEMA_NODE))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a GroundedProcedureNode, got %s", tname.c_str());
	}
	init();
}

void GroundedSchemaNode::init()
{
	_runner = nullptr;

	// Get the schema name.
	const std::string& schema = get_name();

	// At this point, we only run scheme and python schemas.
	if (0 == schema.compare(0, 4, "scm:", 4))
	{
		// Be friendly, and strip leading white-space, if any.
		size_t pos = 4;
		while (' ' == schema[pos]) pos++;
		_runner = new SCMRunner(schema.substr(pos));
		return;
	}

	// At this point, we only run scheme, python schemas and functions from
	// libraries loaded at runtime.
	if (0 == schema.compare(0, 3, "py:", 3))
	{
#ifdef HAVE_CYTHON
		size_t pos = 3;
		while (' ' == schema[pos]) pos++;
		_runner = new PythonRunner(schema.substr(pos));
#else
		throw RuntimeException(TRACE_INFO,
		                       "Cannot evaluate python GroundedSchemaNode!");
#endif /* HAVE_CYTHON */
		return;
	}

	if (0 == schema.compare(0, 4, "lib:", 4))
	{
		_runner = new LibraryRunner(schema);
		return;
	}
}

GroundedSchemaNode::~GroundedSchemaNode()
{
	if (_runner) delete _runner;
}

/// execute -- execute the SchemaNode of the ExecutionOutputLink
///
/// Expects "cargs" to be a ListLink unless there is only one argument
/// Executes the GroundedSchemaNode, supplying cargs as arguments
///
ValuePtr GroundedSchemaNode::execute(AtomSpace* as,
                                     const Handle& cargs,
                                     bool silent)
{
	LAZY_LOG_FINE << "Execute gsn: " << to_short_string()
	              << "with arguments: " << oc_to_string(cargs);

	if (_runner) return _runner->execute(as, cargs, silent);

	// Unknown procedure type
	throw RuntimeException(TRACE_INFO,
	                       "Cannot evaluate unknown Schema %s",
	                       to_short_string().c_str());
}

DEFINE_NODE_FACTORY(GroundedSchemaNode, GROUNDED_SCHEMA_NODE)
