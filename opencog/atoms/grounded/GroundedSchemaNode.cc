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
#include <opencog/atoms/execution/Force.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/PythonEval.h>
#include <opencog/guile/SchemeEval.h>

#include <opencog/atoms/grounded/GroundedSchemaNode.h>
#include "LibraryManager.h"
#include "Runner.h"
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
	}
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

	// Force execution of the arguments. We have to do this, because
	// the user-defined functions are black-boxes, and cannot be trusted
	// to do lazy execution correctly. Right now, forcing is the policy.
	// We could add "scm-lazy:" and "py-lazy:" URI's for user-defined
	// functions smart enough to do lazy evaluation.
	Handle args = force_execute(as, cargs, silent);

	// Get the schema name.
	const std::string& schema = get_name();

	// Extract the language, library and function
	std::string lang, lib, fun;
	LibraryManager::parse_schema(schema, lang, lib, fun);

	ValuePtr result;

	// At this point, we only run scheme, python schemas and functions from
	// libraries loaded at runtime.
	if (lang == "py")
	{
#ifdef HAVE_CYTHON
		// Get a reference to the python evaluator.
		PythonEval &applier = PythonEval::instance();
		result = applier.apply_v(as, fun, args);
#else
		throw RuntimeException(TRACE_INFO,
		                       "Cannot evaluate python GroundedSchemaNode!");
#endif /* HAVE_CYTHON */
	}
	// Used by the Haskel and C++ bindings; can be used with any language
	else if (lang == "lib")
	{
		void* sym = LibraryManager::getFunc(lib,fun);

		// Convert the void* pointer to the correct function type.
		Handle* (*func)(AtomSpace*, Handle*);
		func = reinterpret_cast<Handle* (*)(AtomSpace *, Handle*)>(sym);

		// Execute the function
		Handle* res = func(as, &args);
		if(res != NULL)
		{
			result = *res;
			free(res);
		}
	}
	else
	{
		// Unkown proceedure type
		throw RuntimeException(TRACE_INFO,
		                       "Cannot evaluate unknown Schema %s",
		                       to_short_string().c_str());
	}

	// Check for a not-uncommon user-error.  If the user-defined
	// code returns nothing, then a null-pointer-dereference is
	// likely, a bit later down the line, leading to a crash.
	// So head this off at the pass.
	if (nullptr == result)
	{
		// If silent is true, return a simpler and non-logged
		// exception, which may, in some contexts, will be
		// considerably faster than a RuntimeException.
		if (silent)
			throw NotEvaluatableException();

		throw RuntimeException(TRACE_INFO,
		                       "Invalid return value from schema %s\nArgs: %s",
		                       to_short_string().c_str(),
		                       cargs->to_string().c_str());
	}

	LAZY_LOG_FINE << "Result: " << result->to_string();
	return result;
}

DEFINE_NODE_FACTORY(GroundedSchemaNode, GROUNDED_SCHEMA_NODE)
